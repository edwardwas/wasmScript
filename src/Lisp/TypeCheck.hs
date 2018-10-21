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

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Map             (Map)
import qualified Data.Map             as M
import qualified Data.Set             as S
import           Data.Text            (Text)
import qualified Data.Text            as T

promoteIfSubType :: LispType -> LispType -> Bool
promoteIfSubType a (UnionT as) = a `S.member` as
promoteIfSubType a b           = a == b

throwErrorIf :: MonadError e m => (a -> Bool) -> (a -> e) -> a -> m a
throwErrorIf predicate errFunc a
    | predicate a = throwError $ errFunc a
    | otherwise = pure a

applyFunctionType ::
       MonadError EvalError m => LispType -> LispType -> m LispType
applyFunctionType (FunctionT a b) c
    | promoteIfSubType c a = pure b
    | otherwise = throwError $ TypeMismatch a c
applyFunctionType _ _ = throwError ArityMismatch

typecheckLisp ::
       (MonadState LispTypeCheckState m, MonadError EvalError m)
    => SExpr
    -> m LispType
typecheckLisp (SFunction name args) =
    case builtinAccess builtinTypes name of
        Just funcType ->
            traverse typecheckLisp (expandConsCells args) >>=
            foldM applyFunctionType funcType
typecheckLisp (Atom a) = pure $ typeOfAtom a

typecheckText :: Text -> Either EvalError [Text]
typecheckText t =
    map printType <$>
    runExcept (evalStateT (loadSExpr t >>= traverse typecheckLisp) mempty)
