{-# LANGUAGE DataKinds #-}

module TypeChecker where

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

import CPP.Abs
import CPP.Print
import CPP.ErrM

type Env = (Sig, [Context])	-- functions and context stack
type Sig = Map Id FunType	-- function type signature
type Context = Map Id Type	-- variables with their types
data FunType = FunType { funRet :: Type, funPars :: [Type] }

-- | Builtin-functions
builtin :: [(Id, FunType)]
builtin =
	[ (Id "readInt"    , FunType Type_int    [])
	, (Id "readDouble" , FunType Type_double [])
	, (Id "printInt"   , FunType Type_void [Type_int])
	, (Id "printDouble", FunType Type_void [Type_double])
	]

typecheck :: Program -> Err ()
typecheck (PDefs defs) = do
	let sig = builtin ++ map mkF defs
	checkDefs (Map.fromList sig) defs
	
	where
		mkF (DFun typ id args stms) = (id, FunType typ $ map (\ (ADecl typ _) -> typ) args)

checkDefs :: Sig -> [Def] -> Err ()
checkDefs sig [] = return ()
checkDefs sig ((DFun typ id args stms):defs) = do
	env <- newArgs (sig, [(Map.empty)]) args
	checkStm env typ (SBlock stms)
	checkDefs sig defs

lookFun :: Env -> Id -> Err Type
lookFun (sig, c) id = case (Map.lookup id sig) of
	Just (FunType typ typs)	-> return typ
	Nothing					-> fail $ "lookFun: Function not in Sig"

lookVar :: Env -> Id -> Err Type
lookVar (sig, []) id = fail $ "lookVar: Id not in Env"
lookVar (sig, (c:cs)) id = case (Map.lookup id c) of
	 Just typ	-> return typ
	 Nothing	-> lookVar (sig, cs) id

newFun :: Env -> Id -> FunType -> Err Env
newFun (sig, cs) id ft = case (Map.lookup id sig) of
	Nothing		-> return ((Map.insert id ft sig), cs)
	Just _		-> fail $ "newFun: Function already defined."

newArgs :: Env -> [Arg] -> Err Env
newArgs env []						= return env
newArgs env ((ADecl typ id):args)	= do
	env0 <- newVar env id typ
	newArgs env0 args

newVar :: Env -> Id -> Type -> Err Env
newVar env id Type_void = fail $ "newVar: Variables can't have type void."
newVar env@(sig, (c:cs)) id typ = case (Map.lookup id c) of
	Nothing		-> return (sig, ((Map.insert id typ c):cs))
	Just _		-> fail $ "newVar: Variable already defined."

checkExp :: Env -> Type -> Exp -> Err Env
checkExp env typ exp = do
	typ2 <- inferExp env exp
	if (typ2 == typ) then
		return env
	else
		fail $ "type of " ++ printTree exp

inferExp :: Env -> Exp -> Err Type
inferExp env x = case x of
	ETrue 	-> return Type_bool
	EFalse	-> return Type_bool
	
	EInt a		-> return Type_int
	EDouble a	-> return Type_double
	
	EId id			-> lookVar env id
	EApp id exps	-> lookFun env id
--	EApp id exps	-> do
--		map (\exp -> inferExp env exp) exps
--		lookVar env id
	
	EPostIncr id	-> lookVar env id
	EPostDecr id	-> lookVar env id
	EPreIncr id		-> lookVar env id
	EPreDecr id		-> lookVar env id
	
	ETimes exp0 exp1	-> inferArithm env exp0 exp1
	EDiv exp0 exp1		-> inferArithm env exp0 exp1
	EPlus exp0 exp1		-> inferArithm env exp0 exp1
	EMinus exp0 exp1	-> inferArithm env exp0 exp1
	
	ELt exp0 exp1	-> return Type_bool
	EGt exp0 exp1	-> return Type_bool
	ELtEq exp0 exp1	-> return Type_bool
	EGtEq exp0 exp1	-> return Type_bool
	EEq exp0 exp1	-> return Type_bool
	ENEq exp0 exp1	-> return Type_bool
	
	EAnd exp0 exp1	-> return Type_bool
	EOr exp0 exp1	-> return Type_bool
	
	EAss id exp ->	lookVar env id

-- inferArithmBin i boken.
inferArithm :: Env -> Exp -> Exp -> Err Type
inferArithm env a b = do
	typ 	<- inferExp env a
	typ2 	<- inferExp env b
	if (elem typ [Type_int, Type_double]) && (typ == typ2) then
		return typ
	else
		fail $ "inferArithm: type of expression " -- ++ printTree exp


checkStm :: Env -> Type -> Stm -> Err Env
checkStm env ret x = case x of
	SExp exp -> do
		inferExp env exp
		return env
	SDecls typ ids -> case ids of 
		[]		-> return env
		(id:xs) -> do 
			env0 <- newVar env id typ
			checkStm env0 ret (SDecls typ xs)
	SInit typ id exp ->
		newVar env id typ
	SReturn exp -> do
		typ <- inferExp env exp
		if (typ == ret) then return env
		else fail $ "checkStm: Invalid return type."
	SWhile exp stm -> do
		checkExp env Type_bool exp
		checkStm env ret stm
	SBlock stms -> case stms of
		[]		-> return env
		stm:xs 	-> do
			env0 <- checkStm env ret stm
			checkStm env0 ret (SBlock xs)
	SIfElse exp stm0 stm1 -> do
		checkExp env Type_bool exp
		checkStm env ret stm0
		checkStm env ret stm1


