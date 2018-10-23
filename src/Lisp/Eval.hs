{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Lisp.Eval where

import           Lisp.TypeCheck
import           Lisp.Types
import           Types.Builtins
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

builtinFunctions ::
     MonadError EvalError m => Builtin (M.Map LispType ([SExpr] -> m SExpr))
builtinFunctions =
  Builtin
  { addition =
      [ ( FunctionType floatT (FunctionType floatT floatT)
        , fmap (Atom . FloatA . sum) . traverse (sexprAsType SFloatT))
      , ( FunctionType integerT (FunctionType integerT integerT)
        , fmap (Atom . IntegerA . sum) . traverse (sexprAsType SIntegerT))
      ]
  , equality =
      [ ( FunctionType floatT $ FunctionType floatT boolT
        , fmap (Atom . BoolA . isJust . allEq) . traverse (sexprAsType SFloatT))
      , ( FunctionType integerT $ FunctionType integerT boolT
        , fmap (Atom . BoolA . isJust . allEq) .
          traverse (sexprAsType SIntegerT))
      , ( FunctionType boolT $ FunctionType boolT boolT
        , fmap (Atom . BoolA . isJust . allEq) . traverse (sexprAsType SBoolT))
      ]
  , coerceToFloat =
      [ (FunctionType floatT floatT, pure . head)
      , ( FunctionType integerT floatT
        , fmap (Atom . FloatA . fromIntegral) . sexprAsType SIntegerT . head)
      ]
  }

evalLisp :: (MonadState LispState m, MonadError EvalError m) => SExpr -> m SExpr
evalLisp (SFunction name args) = undefined
evalLisp other                 = error $ show other

