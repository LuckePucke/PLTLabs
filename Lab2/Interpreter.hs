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
		Just (FunDef ids stms)	-> execDef (sig, [Map.empty]) stms

newDefs :: Sig -> [Def] -> Sig
newDefs sig ((DFun typ id args stms):ds)
	= newDefs (Map.insert id (FunDef args stms) sig) ds

newVar :: Env -> Id -> Val -> Env
newVar (sig, (c:cs)) id val = (sig, ((Map.insert id val c):cs))

lookVar :: Env -> Id -> Val
lookVar (sig, c:cs) id = case (Map.lookup id c) of
	Just a -> a
	Nothing -> lookVar (sig, cs) id

updateVar :: Env -> Id -> Val -> Env
updateVar (sig, (c:cs)) id val = case (Map.lookup id c) of
	Just a -> (sig, (Map.insert id val c))
	Nothing -> updateVar (sig, cs) id val

killContext :: Env -> Env
killContext (sig, (c:cs)) = (sig, cs)

newContext :: Env -> Env
newContext (sig, cs) = (sig, (Map.empty:cs))

updateContext :: Context -> [Id] -> [Val] -> Context
updateContext c [] [] = c
updateContext c (id:ids) (val:vals)
	= updateContext (Map.insert id val) ids vals

execMain :: Env -> [Stm] -> IO ()
execMain env [] = IO ()
execMain env (stm:stms) = execMain (evalStm env stm) stms

execFun :: Env -> Id -> [Exp] -> Eval
execFun env id exps = do
	evals <- execFun' env exps
	(env0, val0) <- last evals
	vals <- map (\(e, v) -> v) evals
	(sig, (c:cs)) <- newContext env0
	case (Map.lookup id sig) of
		Just (FunDef args stms) -> do
			context <- updateContext c args vals
			evalStms (sig, (context:cs)) stms

execFun' :: Env -> [Exp] -> [Eval]
execFun' env [] = []
execFun' env (exp:exps) = do
	(env0, val) <- evalExp env exp
	[(env0, val)] ++ execFun' env0 exps

evalStms :: Env -> [Stm] -> Eval
evalStms env [] = (env, VVoid)
evalStms env (stm:stms) = evalStms (evalStm env stm) stms

evalStm :: Env -> Stm -> Eval
evalStm env stm = case stm of
	SExp exp -> do
		evalExp env exp
	SDecls typ ids -> case ids of 
		[]		-> (env, VVoid)
		(id:xs) -> do 
			env0 <- newVar env id VVoid
			evalStm env0 (SDecls typ xs)
	SInit typ id exp -> do
		(env0, val) <- evalExp env exp
		newVar env0 id val
	SReturn exp -> do
		(env0, val) <- evalExp env exp
		env1 <- killContext env0
		(env1, val)
	SWhile exp stm -> do
		(env0, bool) <- evalExp env exp
		case bool of
			VBool True -> evalStm env0 stm
			VBool False -> (env0, VVoid)
	SBlock stms -> do
		env0 <- newContext env
		evalStms env0 stms
	SIfElse exp stm0 stm1 -> do
		(env0, bool) <- evalExp env exp
		case bool of
			VBool True	-> evalStm env0 stm0
			VBool False	-> evalStm env0 stm1

evalExp :: Env -> Exp -> Eval
evalExp env exp = case exp of
	ETrue 	-> (env, VBool True)
	EFalse	-> (env, VBool False)
	
	EInt a		-> (env, VInt a)
	EDouble a	-> (env, VDouble a)
	
	EId id			-> (env, (lookVar env id))
	EApp id exps	-> execFun env id fun
	
	EPostIncr id	-> ((updateVar env id ((lookVar env id)+1)), (lookVar env id))
	EPostDecr id	-> ((updateVar env id ((lookVar env id)-1)), (lookVar env id))
	EPreIncr id		-> ((updateVar env id ((lookVar env id)+1)), (lookVar env id)+1)
	EPreDecr id		-> ((updateVar env id ((lookVar env id)-1)), (lookVar env id)-1)
	
	ETimes exp0 exp1	-> evalExp exp0 * evalExp exp1
	EDiv exp0 exp1		-> inferArithm env exp0 exp1
	EPlus exp0 exp1		-> inferArithm env exp0 exp1
	EMinus exp0 exp1	-> inferArithm env exp0 exp1
	
	ELt exp0 exp1	-> checkArithmLogic env exp0 exp1
	EGt exp0 exp1	-> checkArithmLogic env exp0 exp1
	ELtEq exp0 exp1	-> checkArithmLogic env exp0 exp1
	EGtEq exp0 exp1	-> checkArithmLogic env exp0 exp1
	EEq exp0 exp1	-> checkArithmLogic env exp0 exp1
	ENEq exp0 exp1	-> checkArithmLogic env exp0 exp1
	
	EAnd exp0 exp1	-> checkBoolLogic env exp0 exp1
	EOr exp0 exp1	-> checkBoolLogic env exp0 exp1
	
	EAss id exp -> do

evalArithm :: Env -> Exp -> Exp -> (a -> a -> a) -> Eval 
evalArithm env exp0 exp1 op = do 
	(env0, val0) <- evalExp env exp0
	(env1, val1) <- evalExp env0 exp1
	
