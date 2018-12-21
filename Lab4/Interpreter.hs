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

type Sig = Map Ident FunDef	-- function type signature
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

	EApp e1 e2 -> fail "TODO: EApp e1 e2"
	-- eval e2
	-- smäll in någe (e2?) i cxt
	-- return $ eval e1

	EAdd e1 e2 -> fail "TODO"
	ESub e1 e2 -> fail "TODO"
	ELt e1 e2 -> fail "TODO"
	EIf cond te fe -> fail "TODO"
	EAbs id e -> fail "TODO"

		

evalEntry :: Cxt -> Entry -> Err Value
evalEntry cxt (Val v) = return v
evalEntry cxt (Clos e env) = eval (cxt { cxtEnv = env }) e

newDefs :: Sig -> [Def] -> Sig
newDefs sig [] = sig
newDefs sig ((DDef id ids exp):ds) = newDefs (Map.insert id (FunDef ids exp) sig) ds





