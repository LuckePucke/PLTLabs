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

-- | Error monad.

type Err = Except String

-- | Entry point: Program computes a number.

interpret :: Strategy -> Program -> Err Integer
interpret strategy (Prog defs (DMain mainExp)) = do
	let sig = newDefs Map.empty defs
	let cxt = Cxt strategy sig Map.empty
	val <- eval cxt mainExp
	case val of
		(VInt i) -> return i
		otherwise -> Bad "main should return an integer."

eval :: Cxt -> Exp -> Err Value
eval cxt exp = case exp of
	EVar id	-> do
		env <- cxtEnv cxt
		case (Map.lookup id env) of
			Just entry	-> return entry
			Nothing		-> Bad "entry not in env"

	EInt i	-> return $ VInt i
	EApp e1 e2 -> Bad "sluta" 

newDefs :: Sig -> [Def] -> Sig
newDefs sig [] = sig
newDefs sig ((DDef id ids exp):ds) = newDefs (Map.insert id (FunDef ids exp)) ds
