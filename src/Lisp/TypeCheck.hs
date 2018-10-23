{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Lisp.TypeCheck where

import           Lisp.Print           (printType)
import           Lisp.Read
import           Lisp.Types
import           Types.Builtins
import           Types.Errors
import           Types.SExpr
import           Types.State

import           Control.Monad.Except
import           Control.Monad.Reader

typeOfSExprHelper :: MonadReader (Builtin LispType) m => SExpr -> ExceptT EvalError m LispType
typeOfSExprHelper (Atom a) = pure $ AtomType $ typeOfAtom a
typeOfSExprHelper (SFunction name args) = do
  builtinTypes <- ask
  case builtinAccess builtinTypes name of
    Just t ->
      traverse typeOfSExprHelper (expandConsCells args) >>=
      withExceptT (UnknownTypeError . show) . applyWithArgs t
    Nothing -> undefined

typeOfSExpr :: (MonadReader (Builtin LispType) m, MonadError EvalError m) => SExpr -> m LispType
typeOfSExpr sexpr = do
  et <- runExceptT $ typeOfSExprHelper sexpr
  case et of
    Right x -> return x
    Left e  -> throwError e

