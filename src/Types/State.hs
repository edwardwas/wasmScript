{-# LANGUAGE TemplateHaskell #-}

module Types.State where

import           Lisp.Types
import           Types.SExpr

import           Control.Lens
import qualified Data.Map     as M
import qualified Data.Text    as T

data LispState = LispState
    { _lispFunctions :: M.Map T.Text LispFunction
    , _lispVals      :: [M.Map T.Text SExpr]
    } deriving (Eq,Show)

makeLenses ''LispState

getLispVal :: LispState -> T.Text -> Maybe SExpr
getLispVal ls t = ls ^? lispVals . traverse . ix t

instance Semigroup LispState where
  LispState f1 v1 <> LispState f2 v2 = LispState (f1 <> f2) (v1 <> v2)

instance Monoid LispState where
  mappend = (<>)
  mempty = LispState mempty mempty

data LispTypeCheckState = LispTypeCheckState
    { _lispTypeFunctions :: M.Map T.Text LispType
    , _lispTypeVals      :: [M.Map T.Text LispType]
    } deriving (Eq,Show)
makeLenses ''LispTypeCheckState

instance Semigroup LispTypeCheckState where
    LispTypeCheckState f1 v1 <> LispTypeCheckState f2 v2 =
        LispTypeCheckState (f1 <> f2) (v1 <> v2)

instance Monoid LispTypeCheckState where
  mappend = (<>)
  mempty = LispTypeCheckState mempty mempty
