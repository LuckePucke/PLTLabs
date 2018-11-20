module TypeChecker where

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

import CPP.Abs
import CPP.Print
import CPP.ErrM


typecheck :: Program -> Err ()
typecheck p = return ()

checkExp :: Env -> Type -> Exp -> Err ()
checkExp env typ exp = do
	typ2 <- inferExp env exp
	if (typ2 == typ) then
		return ()
	else
		fail $ "type of " ++ printTree exp

inferExp :: Env -> Exp -> Err Type
inferExp env exp = case x of
	ETrue 	-> return Type_bool
	EFalse	-> return Type_bool
	
	EInt a		-> return Type_int
	EDouble a	-> return Type_double
	
	EId id		-> lookVar env id
--	EApp id exps	-> ples help
	
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

inferArithm :: Env -> Exp -> Exp -> Err Type
inferArithm env a b = do
	typ <- inferExp env a
	if elem typ [Type_int, Type_double] then
		checkExp env b typ
	else
		fail $ "type of expression " ++ printTree exp

-- Det blev 120% förvirring precis innan vi lämnade detta.
checkStm :: Env -> Type -> Exp -> Err Env
checkStm env val x = case x of
	SExp exp -> do
		inferExp env exp
		return env
	SDecls typ ids ->
		map (\id -> updateVar env id typ) ids
	SInit typ id exp -> do
		if (checkExp env exp == typ) then
			updateVar env id typ
		else
			fail $ "type of expression " ++ printTree exp
	SReturn exp ->
		return env

updateVar :: Env -> Id -> Type -> Err ([Type], Type)

