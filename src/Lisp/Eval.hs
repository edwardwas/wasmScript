{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Lisp.Eval where

import           Types.Errors
import           Types.SExpr
import           Types.State
import           Util

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map             as M
import           Data.Maybe           (isJust)

type LispEvalFunc = forall m . (MonadState LispState m, MonadError EvalError m) =>
      [SExpr] -> m SExpr

addition :: LispEvalFunc
addition = fmap (Atom . FloatA . sum) . traverse (sexprAsType SFloatT)

multiplication :: LispEvalFunc
multiplication = fmap (Atom . FloatA . product) . traverse (sexprAsType SFloatT)

negation :: LispEvalFunc
negation [] = pure $ Atom $ FloatA 0
negation inputs = do
  a:as <- traverse (sexprAsType SFloatT) inputs
  return $ Atom $ FloatA (a - sum as)

equality :: [Double] -> SExpr
equality as = Atom $ BoolA $ isJust $ allEq as

defineFunction :: LispEvalFunc
defineFunction [Atom (Symbol funcName), toReplace, body] = do
  args <- traverse (sexprAsType SSymbolT) $ expandConsCells toReplace
  lispFunctions . at funcName .= Just (LispFunction False args body)
  return $ Atom SNil
defineFunction other = error $ show other

condition :: LispEvalFunc
condition (a:as) = do
    let [check, val] = expandConsCells a
    shouldDoHere <- evalLisp check >>= (sexprAsType SBoolT)
    if shouldDoHere
        then evalLisp val
        else condition as
condition [] = error "[]"

evalLisp :: (MonadState LispState m, MonadError EvalError m) => SExpr -> m SExpr
evalLisp (SFunction name args) =
    case name of
        "+" -> traverse evalLisp (expandConsCells args) >>= addition
        "-" -> traverse evalLisp (expandConsCells args) >>= negation
        "*" -> traverse evalLisp (expandConsCells args) >>= multiplication
        "=" -> equality <$> traverse (sexprAsType SFloatT <=< evalLisp) (expandConsCells args)
        "cond" -> condition $ expandConsCells args
        "defn" -> defineFunction $ expandConsCells args
        other -> do
            mLispFunction <- preuse (lispFunctions . ix other)
            case mLispFunction of
                Nothing -> throwError $ CouldNotFindFunction other
                Just (LispFunction isMacro symsToReplace body) -> do
                    args' <-
                        if isMacro
                            then pure (expandConsCells args)
                            else traverse evalLisp (expandConsCells args)
                    lispVals %= (M.fromList (zip symsToReplace args') :)
                    res <- evalLisp body
                    lispVals %= tail
                    return res
evalLisp (Atom (Symbol t)) = do
  mVal <- gets (\ls -> getLispVal ls t)
  case mVal of
    Just v  -> return v
    Nothing -> throwError $ CouldNotFindVal t
evalLisp (Atom a) = pure $ Atom a
evalLisp other = error $ show other

