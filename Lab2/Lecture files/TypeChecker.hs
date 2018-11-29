{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- | Type checker for C-- programs.

module TypeChecker where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Data.Functor
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import CPP.Abs
import CPP.Print (printTree)
import CPP.ErrM  (Err(Ok,Bad))

-- | The signature maps function identifiers to the list of their parameters and the return type.
type Sig     = Map Id FunType
data FunType = FunType { funRet :: Type, funPars :: [Type] }

-- | The state consists of the local context and the return type of the function.
data St = St
  { stCxt :: Cxt   -- ^ Variable bindings.
  , stRet :: Type  -- ^ Return type.
  }

-- | The local context is a stack of typing environments.
type Cxt   = [Block]
type Block = Map Id Type

-- | Type errors are just strings.
type TypeError = String

-- | The type checking monad
type Check = ReaderT Sig (StateT St (Except TypeError))

-- | Builtin-functions
builtin :: [(Id, FunType)]
builtin =
  [ (Id "readInt"    , FunType Type_int    [])
  , (Id "readDouble" , FunType Type_double [])
  , (Id "printInt"   , FunType Type_void [Type_int])
  , (Id "printDouble", FunType Type_void [Type_double])
  ]

-- | Entry point of type checker.
typecheck :: Program -> Err ()
typecheck prg@(PDefs defs) = do
  -- Prepare signature.
  let sig = builtin ++ map mkF defs
      mkF (DFun t f args _ss) = (f, FunType t $ map (\ (ADecl t _) -> t) args)
  -- Check for duplicate function definitions.
  -- TODO!

  -- Prepare the initial state.
  let st = undefined -- St [] $ error "no return type set"
  -- Run the type checker.
  case runExcept (evalStateT (runReaderT (checkProgram prg) $ Map.fromList sig) st) of
    Left s   -> Bad s
    Right () -> return ()

-- | Check program: check definitions and main function.
checkProgram :: Program -> Check ()
checkProgram (PDefs defs) = do
  mapM_ checkDef defs
  -- TODO: checkMain

-- | Check a single function definition.
checkDef :: Def -> Check ()
checkDef (DFun t f args ss) = do
  -- Set initial context and return type.
  put $ St [Map.empty] t
  -- Add function parameters to the context.
  mapM_ (\ (ADecl t x) -> newVar x t) args
  -- Check function body.
  mapM_ checkStm ss

-- | Check a single statement; mutates the context.
checkStm :: Stm -> Check ()
checkStm = \case
  SInit t x e -> do
    checkExp e t
    newVar x t
  s -> nyi

-- | Infer the type of an expression.
inferExp :: Exp -> Check Type
inferExp = \case
  EInt i -> return Type_int
  EApp f es -> do
    sig <- ask
    case Map.lookup f sig of
      Nothing -> throwError $ "function " ++ printTree f ++ " unbound"
      Just (FunType t ts) -> do
        unless (length es == length ts) $
          throwError $ "wrong number of arguments"
        zipWithM_ checkExp es ts
        return t
  e -> nyi

-- | Check an expression against a given type.
checkExp :: Exp -> Type -> Check ()
checkExp e t = do
  t' <- inferExp e
  unless (t == t') $ throwError $
    "expected type " ++ printTree t ++ ", but inferred type is " ++ printTree t'

nyi = error "NOT YET IMPLEMENTED"

-- * Variable handling

-- | Update the typing context.
modifyCxt :: (Cxt -> Cxt) -> Check ()
modifyCxt f = modify $ \ (St bs t) -> St (f bs) t

-- | Add a new binding and make sure it is unique in the top context block.
newVar :: Id -> Type -> Check ()
newVar x t = do
  when (t == Type_void) $
    throwError $ "type void of variable " ++ printTree x ++ " is illegal"
  (b:bs) <- gets stCxt
  let (found, b') = Map.insertLookupWithKey (\ _ t _ -> t) x t b
  unless (isNothing found) $ throwError $ "duplicate variable binding " ++ printTree x
  modifyCxt $ const (b':bs)
