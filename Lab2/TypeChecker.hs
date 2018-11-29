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
typecheck p = return ()

lookVar :: Env -> Id -> Err Type
lookVar (_, []) id = fail $ "lookVar: Id not in Env"
lookVar (sig, ((Map cid ctyp):cs)) id
	| cid == id = return ctyp
	| otherwise = lookVar (sig, cs) id

updateVar :: Env -> Id -> Type -> Err Env
updateVar (sig, cs) id typ = (sig, (updateVar' cs id typ))

updateVar' :: [Context] -> Id -> Type -> [Context]
updateVar' [] id typ = [(Map id typ)]
updateVar' (c:cs) id typ = case c of 
	(Map id typ)	-> (c:cs)
	(Map id _)		-> fail $ "updateVar. Id has a different Type"
	_				-> [c]++(updateVar' cs id typ)

emptyEnv :: Env

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
	EApp id exps	-> lookvar env id
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
	typ <- inferExp env a
	if elem typ [Type_int, Type_double] then
		checkExp env b typ
	else
		fail $ "type of expression " ++ printTree exp

checkStm :: Env -> Type -> Exp -> Err Env
checkStm env val x = case x of
	SExp exp -> do
		inferExp env exp
		return env
	SDecls typ ids ->
		map (\id -> updateVar env id typ) ids
	SInit typ id exp ->
		updateVar env id typ
	SReturn exp -> do
		inferExp env exp
		return env
	SWhile exp stm -> do
		checkExp env Type_bool exp
		checkStm env val stm
	SBlock stms ->
		map (\stm -> checkStm env val stm) stms
	SIfElse exp stm0 stm1 -> do
		checkExp env Type_bool exp
		checkStm env val stm0
		checkStm env val stm1


