{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Lisp.TypeCheck where

import           Lisp.Print           (printType)
import           Lisp.Read
import           Lisp.Types
import           Types.Errors
import           Types.SExpr
import           Types.State

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Text            (Text)
import qualified Data.Text            as T

throwErrorIf :: MonadError e m => (a -> Bool) -> (a -> e) -> a -> m a
throwErrorIf predicate errFunc a
    | predicate a = throwError $ errFunc a
    | otherwise = pure a

numericFunction ::
       (MonadState LispTypeCheckState  m, MonadError EvalError m)
    => SExpr
    -> m LispType
numericFunction args = do
    traverse typecheckLisp (expandConsCells args) >>=
        mapM_ (throwErrorIf (/= F64T) (TypeMismatch F64T))
    return F64T

typecheckLisp ::
       (MonadState LispTypeCheckState m, MonadError EvalError m)
    => SExpr
    -> m LispType
typecheckLisp (SFunction name args) =
    case name of
        "+" -> numericFunction args
        "-" -> numericFunction args
        "*" -> numericFunction args
        "cond" -> condHelper $ expandConsCells args
        "defn" ->
            case expandConsCells args of
                [Atom (Symbol _), toReplace, body] -> undefined
                _ -> throwError ArityMismatch
        _ -> undefined
typecheckLisp (Atom (F64A _)) = pure F64T
typecheckLisp (Atom (Symbol s)) = do
    mType <- preuse (lispTypeVals . traverse . ix s)
    case mType of
        Just t  -> return t
        Nothing -> throwError $ CouldNotFindVal s
typecheckLisp (Atom (BoolA _)) = pure BoolT
typecheckLisp (Atom SNil) = pure NilT
typecheckLisp _other = undefined

condHelper ::
       (MonadState LispTypeCheckState m, MonadError EvalError m)
    => [SExpr]
    -> m LispType
condHelper [a] = do
    [checkT, valT] <-
        traverse typecheckLisp (expandConsCells a) >>=
        throwErrorIf ((/= 2) . length) (const ArityMismatch)
    unless (checkT == BoolT) $ throwError $ TypeMismatch BoolT checkT
    return valT
condHelper (a:as) = do
    [checkT, valT] <-
        traverse typecheckLisp (expandConsCells a) >>=
        throwErrorIf ((/= 2) . length) (const ArityMismatch)
    unless (checkT == BoolT) $ throwError $ TypeMismatch BoolT checkT
    otherT <- condHelper as
    unless (otherT == valT) $ throwError $ TypeMismatch otherT valT
    return otherT
condHelper [] = throwError ArityMismatch

typecheckText :: Text -> Either EvalError [Text]
typecheckText t =
    map printType <$>
    runExcept (evalStateT (loadSExpr t >>= traverse typecheckLisp) mempty)
