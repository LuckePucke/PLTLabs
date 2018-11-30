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
	sig <- newFuns (Map.fromList builtin) defs
	checkMain sig
	checkDefs sig defs
	{-
	++ map mkF defs
	
	where
		mkF (DFun typ id args stms) = (id, FunType typ $ map (\ (ADecl typ _) -> typ) args)
		-}

checkMain :: Sig -> Err ()
checkMain sig = case (Map.lookup (Id "main") sig) of
	Just (FunType Type_int []) -> return ()
	otherwise -> fail $ "fukc off"

checkDefs :: Sig -> [Def] -> Err ()
checkDefs sig [] = return ()
checkDefs sig ((DFun typ id args stms):defs) = do
	env <- newArgs (sig, [(Map.empty)]) args
	checkStm env typ (SBlock stms)
	checkDefs sig defs

lookFun :: Env -> Id -> [Exp] -> Err Type
lookFun env@(sig, c) id exps = case (Map.lookup id sig) of
	Just (FunType typ typs)	-> lookFun' env typ typs exps
	Nothing					-> fail $ "lookFun: Function not in Sig."

lookFun' :: Env -> Type -> [Type] -> [Exp] -> Err Type
lookFun' env typ [] []	= return typ
lookFun' env typ [] _	= fail $ "lookFun': Too many arguments."
lookFun' env typ _ []	= fail $ "lookFun': Too few arguments."
lookFun' env typ (t:ts) (e:es) = do
	t0 <- inferExp env e
	if (t0 == t) then
		lookFun' env typ ts es
	else
		fail $ "lookFun': Argument has wrong type."

lookVar :: Env -> Id -> Err Type
lookVar (sig, []) id = fail $ "lookVar: Id not in Env"
lookVar (sig, (c:cs)) id = case (Map.lookup id c) of
	 Just typ	-> return typ
	 Nothing	-> lookVar (sig, cs) id

newFuns :: Sig -> [Def] -> Err Sig
newFuns sig [] = return sig
newFuns sig (d:ds) = do
	sig0 <- newFun sig d
	newFuns sig0 ds

newFun :: Sig -> Def -> Err Sig
newFun sig (DFun typ id args stms) = case (Map.lookup id sig) of
	Nothing		-> return (Map.insert id (FunType typ (newFun' args)) sig)
	Just _		-> fail $ "newFun: Function already defined."

newFun' :: [Arg] -> [Type]
newFun' [] = []
newFun' ((ADecl typ id):as) = [typ] ++ newFun' as

newArgs :: Env -> [Arg] -> Err Env
newArgs env []						= return env
newArgs env ((ADecl typ id):args)	= do
	env0 <- newVar env id typ
	newArgs env0 args

newVar :: Env -> Id -> Type -> Err Env
newVar env id Type_void = fail $ "newVar: Variables can't have type void."
newVar env@(sig, (c:cs)) id typ = do
	newVar' (c:cs) id
	case (Map.lookup id c) of
		Nothing	-> return (sig, ((Map.insert id typ c):cs))
		Just _	-> fail $ "newVar: Variable already defined."

newVar' :: [Context] -> Id -> Err ()
newVar' [c] id = case (Map.lookup id c) of
	Just _ -> fail $ "NewVar': Variable name used by function argument."
	Nothing -> return ()
newVar' (c:cs) id = newVar' cs id

checkExp :: Env -> Type -> Exp -> Err ()
checkExp env typ exp = do
	typ2 <- inferExp env exp
	if (typ2 == typ) then
		return ()
	else
		fail $ "type of " ++ printTree exp

inferExp :: Env -> Exp -> Err Type
inferExp env x = case x of
	ETrue 	-> return Type_bool
	EFalse	-> return Type_bool
	
	EInt a		-> return Type_int
	EDouble a	-> return Type_double
	
	EId id			-> lookVar env id
	EApp id exps	-> lookFun env id exps
	
	EPostIncr id	-> checkInt env id
	EPostDecr id	-> checkInt env id
	EPreIncr id		-> checkInt env id
	EPreDecr id		-> checkInt env id
	
	ETimes exp0 exp1	-> inferArithm env exp0 exp1
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
		typ0 <- lookVar env id
		typ1 <- inferExp env exp
		if (typ0 == typ1) then
			return typ0
		else
			fail $ "inferExp: Trying to assign wrong type to variable."

checkInt :: Env -> Id -> Err Type
checkInt env id = do
	typ <- lookVar env id
	case typ of
		Type_int -> return typ
		otherwise -> fail $ "checkInt: Variable not an integer."

checkArithmLogic :: Env -> Exp -> Exp -> Err Type
checkArithmLogic env exp0 exp1 = do
	typ <- inferArithm env exp0 exp1
	return Type_bool

checkBoolLogic :: Env -> Exp -> Exp -> Err Type
checkBoolLogic env exp0 exp1 = do
	typ 	<- inferExp env exp0
	typ2 	<- inferExp env exp1
	if (typ == Type_bool) && (typ2 == Type_bool) then
		return Type_bool
	else
		fail $ "CheckBoolLogic: Both not bool." -- ++ printTree exp



-- inferArithmBin i boken.
inferArithm :: Env -> Exp -> Exp -> Err Type
inferArithm env a b = do
	typ 	<- inferExp env a
	typ2 	<- inferExp env b
	if (elem typ [Type_int, Type_double]) && (typ == typ2) then
		return typ
	else
		fail $ "inferArithm: Not same type of numeric." -- ++ printTree exp


checkStm :: Env -> Type -> Stm -> Err Env
checkStm env@(sig, (c:cs)) ret x = case x of
	SExp exp -> do
		inferExp env exp
		return env
	SDecls typ ids -> case ids of 
		[]		-> return env
		(id:xs) -> do 
			env0 <- newVar env id typ
			checkStm env0 ret (SDecls typ xs)
	SInit typ id exp -> do
		checkExp env typ exp
		newVar env id typ
	SReturn exp -> do
		typ <- inferExp env exp
		if (typ == ret) then return env
		else fail $ "checkStm: Invalid return type."
	SWhile exp stm -> do
		checkExp env Type_bool exp
		checkStm env ret (SBlock [stm])
	SBlock stms -> do
		env0 <- newBlock env
		checkBlock env0 ret stms
	SIfElse exp stm0 stm1 -> do
		checkExp env Type_bool exp
		checkStm env ret (SBlock [stm0])
		checkStm env ret (SBlock [stm1])

newBlock :: Env -> Err Env
newBlock (sig, cs) = return (sig, (Map.empty:cs))

checkBlock :: Env -> Type -> [Stm] -> Err Env
checkBlock (sig, (c:cs)) typ [] = return (sig, cs)
checkBlock env typ (stm:xs) = do
	env0 <- checkStm env typ stm
	checkBlock env0 typ xs


