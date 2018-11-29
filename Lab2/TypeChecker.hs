{-# LANGUAGE DataKinds #-}

module TypeChecker where

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

import CPP.Abs
import CPP.Print
import CPP.ErrM

type Env = (Sig, [Context])			-- functions and context stack
type Sig = Map Id ([Type], Type)	-- function type signature
type Context = Map Id Type			-- variables with their types

typecheck :: Program -> Err ()
typecheck (PDefs defs) = checkDefs defs

-- | Builtin-functions
builtin :: [Sig]
builtin =
  [ Map (Id "readInt") ([], Type_int)
  , Map (Id "readDouble")   ([], Type_double)
  , Map (Id "printInt") ([Type_int], Type_void )
  , Map (Id "printDouble") ([Type_double], Type_void )
  ]


--createEnv :: Id -> Type -> Err Env
--createEnv id typ = return [((id, ([], typ)), [])]

checkDefs :: [Def] -> Err ()
checkDefs [] = return ()
checkDefs ((DFun typ id args stms):defs) = do
			env <- createEnv id typ
			env <- updateArgs env args
			env <- checkStm env (SBlock stms)
			return ()

lookVar :: Env -> Id -> Err Type
lookVar (_, []) id = fail $ "lookVar: Id not in Env"
lookVar (sig, (con:cs)) id = case (Map.lookup id con) of
	 Just typ	= return typ
	 Nothing	= lookVar (sig, cs) id

updateArgs :: Env -> [Arg] -> Err Env
updateArgs env [] 						= return env
updateArgs env ((ADecl typ id):args) 	= updateVar env id typ 

updateVar :: Env -> Id -> Type -> Err Env
updateVar (sig, cs) id typ = return (sig, (updateVar' cs id typ))

updateVar' :: [Context] -> Id -> Type -> [Context]
updateVar' [] id typ 		= [(id, typ)]
updateVar' (c:cs) id typ 	= case c of 
	(id, typ)	-> (c:cs)
	(id, _)		-> fail $ "updateVar. Id has a different Type"
	_			-> [c]++(updateVar' cs id typ)


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
	EApp id exps	-> lookVar env id
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


checkStm :: Env -> Stm -> Err Env
checkStm env x = case x of
	SExp exp -> do
		inferExp env exp
		return env
	SDecls typ ids -> case ids of 
		[] 		-> return env
		(id:xs) -> do 
			updateVar env id typ
			checkStm env (SDecls typ xs)
	SInit typ id exp ->
		updateVar env id typ
	SReturn exp -> do
		inferExp env exp
		return env
	SWhile exp stm -> do
		checkExp env Type_bool exp
		checkStm env stm
	SBlock stms -> case stms of
		[]		-> return env
		stm:xs 	-> do
			checkStm env stm
			checkStm env (SBlock xs)
	SIfElse exp stm0 stm1 -> do
		checkExp env Type_bool exp
		checkStm env stm0
		checkStm env stm1


