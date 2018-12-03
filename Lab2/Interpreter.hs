module Interpreter where

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

import CPP.Abs
import CPP.Print
import CPP.ErrM

type Env = (Sig, [Context])
type Sig = Map Id FunDef	-- function type signature
type Context = Map Id Val	-- variables with their types
data FunDef = FunDef { funArgs :: [Id], funStms :: [Stm] }
type Eval = (Env, Val)

data Val
	= VBool		Bool
	| VInt		Integer
	| VDouble	Double
	| VVoid

interpret :: Program -> IO ()
interpret (PDefs defs) = do
	let sig = newDefs Map.empty defs
	case (Map.lookup (Id "main") sig) of
		Just (FunDef ids stms)	-> execMain (sig, [Map.empty]) stms

newDefs :: Sig -> [Def] -> Sig
newDefs sig [] = sig
newDefs sig ((DFun typ id args stms):ds)
	= newDefs (Map.insert id (FunDef (newDefs' args) stms) sig) ds

newDefs' :: [Arg] -> [Id]
newDefs' [] = []
newDefs' ((ADecl t id):as) = [id] ++ newDefs' as

newVar :: Env -> Id -> Val -> Env
newVar (sig, (c:cs)) id val = (sig, ((Map.insert id val c):cs))

lookVar :: Env -> Id -> Val
lookVar (sig, c:cs) id = case (Map.lookup id c) of
	Just a -> a
	Nothing -> lookVar (sig, cs) id

updateVar :: Env -> Id -> Val -> Env
updateVar (sig, (c:cs)) id val = case (Map.lookup id c) of
	Just a	-> (sig, ((Map.insert id val c):cs))
	Nothing -> do
		let (sig0, cs0) = updateVar (sig, cs) id val
		(sig0, (c:cs0))

killContext :: Env -> Env
killContext (sig, (c:cs)) = (sig, cs)

newContext :: Env -> Env
newContext (sig, cs) = (sig, (Map.empty:cs))

execMain :: Env -> [Stm] -> IO ()
execMain env [] = putStr ""
execMain env stms = do
	_ <- evalStms env VVoid True stms
	return ()

execFun :: Env -> Id -> [Exp] -> IO Eval
execFun env id exps = do
	let env0@(sig, cs) = newContext env
	case (Map.lookup id sig) of
		Just (FunDef args stms) -> do
			execFun' env0 exps args stms

execFun' :: Env -> [Exp] -> [Id] -> [Stm] -> IO Eval
execFun' env [] [] stms = do
	(_, (env0, val)) <- evalStms env VVoid True stms
	return ((killContext env0), val)
execFun' (sig, c:cs) (exp:exps) (id:ids) stms = do
	((sig0, cs0), val)	<- evalExp (sig, cs) exp
	let c0				= Map.insert id val c
	execFun' (sig0, (c0:cs0)) exps ids stms

evalStms :: Env -> Val -> Bool -> [Stm] -> IO (Bool, Eval)
evalStms env val bool []			= return (bool, (env, val))
evalStms env val False stms			= return (False, (env, val))
evalStms env val True (stm:stms)	= case stm of
	SExp exp -> do
		(env0, val0) <- evalExp env exp
		evalStms env0 val True stms
	SDecls typ ids -> case ids of 
		[]		-> evalStms env val True stms
		(id:xs) -> do 
			let env0 = newVar env id VVoid
			evalStms env0 val True [(SDecls typ xs)]
	SInit typ id exp -> do
		(env0, val0)	<- evalExp env exp
		let env1		= newVar env0 id val0
		evalStms env1 val True stms
	SReturn exp -> do
		(env0, val0) <- evalExp env exp
		evalStms env0 val0 False stms
	SWhile exp stm0 -> do
		(env0, cond) <- evalExp env exp
		case cond of
			VBool True -> do
				(bool, (env1, val0)) <- evalStms env0 val True [(SBlock [stm0])]
				evalStms env1 val0 bool (stm:stms)
			VBool False -> evalStms env0 val True stms
	SBlock stms0 -> do
		let env0 = newContext env
		(bool, (env1, val0)) <- evalStms env0 val True stms0
		evalStms (killContext env1) val0 bool stms
	SIfElse exp stm0 stm1 -> do
		(env0, cond) <- evalExp env exp
		case cond of
			VBool True	-> do
				(bool, (env1, val0)) <- evalStms env0 val True [(SBlock [stm0])]
				evalStms env1 val0 bool stms
			_	-> do
				(bool, (env1, val0)) <- evalStms env0 val True [(SBlock [stm1])]
				evalStms env1 val0 bool stms

evalExp :: Env -> Exp -> IO Eval
evalExp env exp = case exp of
	ETrue 	-> return (env, VBool True)
	EFalse	-> return (env, VBool False)
	
	EInt a		-> return (env, VInt a)
	EDouble a	-> return (env, VDouble a)
	
	EId id			-> return (env, (lookVar env id))
	EApp id exps	-> case id of
		(Id "printInt") -> do
			let exp = head exps
			(env0, (VInt i)) <- evalExp env exp
			putStrLn (show i)
			return (env0, VInt i)
		(Id "printDouble") -> do
			let exp = head exps
			(env0, (VDouble d)) <- evalExp env exp
			putStrLn (show d)
			return (env0, VDouble d)
		(Id "readInt") -> do
			inp <- getLine
			return (env, VInt (read inp :: Integer))
		(Id "readDouble") -> do
			inp <- getLine
			return (env, VDouble (read inp :: Double))
		otherwise -> execFun env id exps
	
	EPostIncr id	-> return (updateVar env id (incrVal env id), (lookVar env id))
	EPostDecr id	-> return (updateVar env id (decrVal env id), (lookVar env id))
	EPreIncr id		-> return (updateVar env id (incrVal env id), (incrVal env id))
	EPreDecr id		-> return (updateVar env id (decrVal env id), (decrVal env id))
	
	ETimes exp0 exp1	-> do 
		(env0, val0) <- evalExp env exp0
		(env1, val1) <- evalExp env0 exp1
		case (val0, val1) of
			(VInt i0, VInt i1)			-> return (env1, VInt (i0 * i1))
			(VDouble d0, VDouble d1)	-> return (env1, VDouble (d0 * d1))
	EDiv exp0 exp1		-> do 
		(env0, val0) <- evalExp env exp0
		(env1, val1) <- evalExp env0 exp1
		case (val0, val1) of
			(VInt i0, VInt i1)			-> return (env1, VInt (div i0 i1))
			(VDouble d0, VDouble d1)	-> return (env1, VDouble (d0 / d1))
	EPlus exp0 exp1		-> do 
		(env0, val0) <- evalExp env exp0
		(env1, val1) <- evalExp env0 exp1
		case (val0, val1) of
			(VInt i0, VInt i1)			-> return (env1, VInt (i0 + i1))
			(VDouble d0, VDouble d1)	-> return (env1, VDouble (d0 + d1))
	EMinus exp0 exp1	-> do 
		(env0, val0) <- evalExp env exp0
		(env1, val1) <- evalExp env0 exp1
		case (val0, val1) of
			(VInt i0, VInt i1)			-> return (env1, VInt (i0 - i1))
			(VDouble d0, VDouble d1)	-> return (env1, VDouble (d0 - d1))
	
	ELt exp0 exp1 -> do
		(env0, val0) <- evalExp env exp0
		(env1, val1) <- evalExp env0 exp1
		case (val0, val1) of
			(VInt i0, VInt i1)			-> return (env1, VBool (i0 < i1))
			(VDouble d0, VDouble d1)	-> return (env1, VBool (d0 < d1))
	EGt exp0 exp1	-> do
		(env0, val0) <- evalExp env exp0
		(env1, val1) <- evalExp env0 exp1
		case (val0, val1) of
			(VInt i0, VInt i1)			-> return (env1, VBool (i0 > i1))
			(VDouble d0, VDouble d1)	-> return (env1, VBool (d0 > d1))
	ELtEq exp0 exp1	-> do
		(env0, val0) <- evalExp env exp0
		(env1, val1) <- evalExp env0 exp1
		case (val0, val1) of
			(VInt i0, VInt i1)			-> return (env1, VBool (i0 <= i1))
			(VDouble d0, VDouble d1)	-> return (env1, VBool (d0 <= d1))
	EGtEq exp0 exp1	-> do
		(env0, val0) <- evalExp env exp0
		(env1, val1) <- evalExp env0 exp1
		case (val0, val1) of
			(VInt i0, VInt i1)			-> return (env1, VBool (i0 >= i1))
			(VDouble d0, VDouble d1)	-> return (env1, VBool (d0 >= d1))
	EEq exp0 exp1	-> do
		(env0, val0) <- evalExp env exp0
		(env1, val1) <- evalExp env0 exp1
		case (val0, val1) of
			(VInt i0, VInt i1)			-> return (env1, VBool (i0 == i1))
			(VDouble d0, VDouble d1)	-> return (env1, VBool (d0 == d1))
			(VBool b0, VBool b1)		-> return (env1, VBool (b0 == b1))
	ENEq exp0 exp1	-> do
		(env0, val0) <- evalExp env exp0
		(env1, val1) <- evalExp env0 exp1
		case (val0, val1) of
			(VInt i0, VInt i1)			-> return (env1, VBool (i0 /= i1))
			(VDouble d0, VDouble d1)	-> return (env1, VBool (d0 /= d1))
			(VBool b0, VBool b1)		-> return (env1, VBool (b0 /= b1))
	
	EAnd exp0 exp1	-> do
		(env0, val0) <- evalExp env exp0
		case val0 of
			VBool True	-> evalExp env0 exp1
			_				-> return (env0, val0)
	
	EOr exp0 exp1	-> do
		(env0, val0) <- evalExp env exp0
		case val0 of
			VBool False	-> evalExp env0 exp1
			_			-> return (env0, val0)
	
	EAss id exp -> do
		(env0, val) <- evalExp env exp
		return (updateVar env0 id val, val)


incrVal :: Env -> Id -> Val
incrVal env id = case (lookVar env id) of
	VInt int 		-> (VInt (int + 1))
	VDouble double	-> (VDouble (double + 1.0))

decrVal :: Env -> Id -> Val
decrVal env id = case (lookVar env id) of
	VInt int 		-> (VInt (int - 1))
	VDouble double	-> (VDouble (double - 1.0))


