{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Lisp.TypeCheck where

import           Types
import           Util

import           Control.Monad.Except

typecheckLisp :: (MonadError EvalError m) => SExpr -> m LispType
typecheckLisp (SFunction name args) =
    case name of
        "+" -> do
            argTypes <- traverse typecheckLisp $ expandConsCells args
            case allEq argTypes of
                Nothing -> throwError UnknownTypeError
                Just t ->
                    if t == F64T
                        then return t
                        else throwError (CannotGetAsType F64T args)
        "-" -> undefined
        "*" -> undefined
        "cond" -> undefined
        "defn" -> undefined
        _ -> undefined
typecheckLisp (Atom (F64A _)) = pure F64T
typecheckLisp (Atom (Symbol _)) = pure SymbolT
typecheckLisp (Atom (BoolA _)) = pure BoolT
typecheckLisp (Atom SNil) = pure NilT
typecheckLisp _other = undefined
