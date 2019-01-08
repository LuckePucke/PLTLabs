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
import Debug.Trace

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
	| VClos Ident Exp Env
instance Show Value where
	show (VInt i) = "VInt " ++ show i
	show (VClos id exp env) = "VClos " ++ show id ++ " " ++ show exp

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
		VInt i -> return i
		VClos{} -> fail $ "main: not int."
--		(VClos id exp env) -> do
--			let cxt' = cxt {cxtEnv = env}
--			val' <- eval cxt' exp
--			evalValue cxt' val'

newEnv :: Cxt -> Cxt
newEnv cxt = cxt { cxtEnv = Map.empty }

eval :: Cxt -> Exp -> Err Value
eval cxt exp = case exp of

	EVar id	-> case (Map.lookup id (cxtEnv cxt)) of
		Just entry	-> evalEntry cxt entry
		Nothing		-> case (Map.lookup id (cxtSig cxt)) of
			Just (FunDef ids e) -> eval (newEnv cxt) e
			Nothing -> fail $ "entry " ++ show id ++ " not in env or sig"

	EInt i	-> return $ VInt i

	EApp fun args -> do
		
		clos <- eval cxt fun
		case clos of
			VClos id funExp env -> trace (show id ++ " : " ++ show args ++ " = " ++ show funExp) $ case (cxtStrategy cxt) of
				
				CallByName -> do
					let entry = Clos args env
					eval cxt { cxtEnv = Map.insert id entry env } funExp
				
				CallByValue -> do
					val <- eval cxt args
					let cxt' = cxt { cxtEnv = Map.insert id (Val val) env } 
					eval cxt' funExp
			
			_ -> fail $ "should be function."
	
	EAdd e1 e2 -> do
		a <-  eval cxt e1
		b <-  eval cxt e2
		a' <- evalValue cxt a
		b' <- evalValue cxt b
		trace ("EAdd\n") (return $ VInt (a' + b'))

	ESub e1 e2 -> do
		a <-  eval cxt e1
		b <-  eval cxt e2
		a' <- evalValue cxt a
		b' <- evalValue cxt b
		trace ("ESub\n") (return $ VInt (a' - b'))

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

	EAbs id e -> return $ VClos id e (cxtEnv cxt)


evalValue :: Cxt -> Value -> Err Integer
evalValue cxt (VInt i) = return i
evalValue cxt (VClos id e env) = do
	val <- eval (cxt { cxtEnv = env }) e
	evalValue cxt val 

evalEntry :: Cxt -> Entry -> Err Value
evalEntry cxt (Val v) = return v
evalEntry cxt (Clos e env) = eval (cxt { cxtEnv = env }) e

newDefs :: Sig -> [Def] -> Sig
newDefs sig [] = sig
newDefs sig ((DDef id ids exp):ds) = newDefs (Map.insert id (FunDef ids exp) sig) ds





