-- | Interpreter for lambda-calculus with if, +, -, <.
--
--   Strategy can be either call-by-value or call-by-name.

{-# LANGUAGE LambdaCase #-}

module Interpreter (interpret, Strategy(..)) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Data.Functor
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Hgrammar.Abs
import Hgrammar.Print
import Hgrammar.ErrM

-- | Evaluation strategy.

data Strategy
  = CallByName
  | CallByValue

-- | Context

type Sig = Map Ident FunDef
data FunDef = FunDef [Ident] Exp
type Env = Map Ident Entry
data Entry
	= Val Value
	| Clos Exp Env
data Value
	= VInt Integer
	| VClos Exp Env

data Cxt = Cxt
	{ cxtStrategy :: Strategy
	, cxtSig :: Sig
	, cxtEnv :: Env
	}

-- | Entry point: Program computes a number.

interpret :: Strategy -> Program -> Err Integer
interpret strategy (Prog defs (DMain mainExp)) = do
	let sig = newDefs Map.empty defs
	let cxt = Cxt strategy sig Map.empty
	val <- eval cxt mainExp
	case val of
		(VInt i) -> return i
		otherwise -> fail "main should return an integer."

eval :: Cxt -> Exp -> Err Value
eval cxt exp = case exp of

	EVar id	-> case (Map.lookup id (cxtEnv cxt)) of
		Just entry	-> evalEntry cxt entry
		Nothing		-> fail "entry not in env"

	EInt i	-> return $ VInt i

	EApp e1 e2 -> evalApp (cxt {cxtEnv = Map.empty}) [] (EApp e1 e2)

	EAdd e1 e2 -> do
		a <-  eval cxt e1
		b <-  eval cxt e2
		a' <- evalValue cxt a
		b' <- evalValue cxt b
		return $ VInt (a' + b')

	ESub e1 e2 -> do
		a <-  eval cxt e1
		b <-  eval cxt e2
		a' <- evalValue cxt a
		b' <- evalValue cxt b
		return $ VInt (a' - b')

	ELt e1 e2 -> do
		a <-  eval cxt e1
		b <-  eval cxt e2
		a' <- evalValue cxt a
		b' <- evalValue cxt b
		if a' < b'
			then return $ VInt 1
			else return $ VInt 0

	EIf cond te fe -> do
		cv <- eval cxt cond
		c <- evalValue cxt cv
		case c of
			1 -> eval cxt te
			0 -> eval cxt fe
		-- öööh behöver vi bry oss om name vs value skiten?

	EAbs id e -> fail "EAbs needs to be handled by a EApp"
		--do
--		let env = cxtEnv cxt
--		Map.insert id (

evalApp :: Cxt -> [Value] -> Exp -> Err Value
evalApp cxt vals (EVar id) = case Map.lookup id (cxtSig cxt) of
	Just (FunDef ids exp) -> do
		cxt' <- evalApp' cxt ids vals
		eval cxt' exp
	otherwise -> fail "Fun id not a def in sig."
evalApp cxt [val] (EAbs id exp) = do
	cxt' <- evalApp' cxt [id] [val]
	eval cxt' exp
evalApp cxt vals (EAbs id exp) = fail "More than one argument provided to lambda fun."
evalApp cxt vals (EApp e1 e2) = do
	val <- eval cxt e2
	evalApp cxt (val:vals) e1

evalApp' :: Cxt -> [Ident] -> [Value] -> Err Cxt
evalApp' cxt [] [] = return cxt
evalApp' cxt [] _ = fail "Given too many arguments."
evalApp' cxt _ [] = fail "Given too few arguments."
evalApp' cxt (id:ids) (v:vs) = evalApp' (cxt {cxtEnv = (Map.insert id (Val v) (cxtEnv cxt))}) ids vs

evalValue :: Cxt -> Value -> Err Integer
evalValue cxt (VInt i) = return i
evalValue cxt (VClos e env) = do
	val <- eval (cxt { cxtEnv = env }) e
	evalValue cxt val 

evalEntry :: Cxt -> Entry -> Err Value
evalEntry cxt (Val v) = return v
evalEntry cxt (Clos e env) = eval (cxt { cxtEnv = env }) e

newDefs :: Sig -> [Def] -> Sig
newDefs sig [] = sig
newDefs sig ((DDef id ids exp):ds) = newDefs (Map.insert id (FunDef ids exp) sig) ds





