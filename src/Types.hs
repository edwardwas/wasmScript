{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}

module Types where

import           Control.Lens
import           Control.Monad.Except
import qualified Data.Map             as M
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Void
import           Text.Megaparsec      (ParseError)

tShow :: Show a => a -> Text
tShow = T.pack . show

data Atom
    = F64A Double
    | Symbol Text
    | BoolA Bool
    | SNil
      deriving (Eq,Show)

data LispType
    = F64T
    | SymbolT
    | BoolT
    | NilT
    | PairT LispType LispType
    deriving (Eq, Show)

data SLispType (t :: LispType) where
  SF64T :: SLispType 'F64T
  SSymbolT :: SLispType 'SymbolT
  SBoolT :: SLispType 'BoolT
  SNilT :: SLispType 'NilT

class AsLispType t where
    type family HaskellType t :: *
    singLispType :: SLispType t
    sexprAsType :: MonadError EvalError m => proxy t -> SExpr -> m (HaskellType t)

instance AsLispType 'F64T where
  type HaskellType 'F64T = Double
  singLispType = SF64T
  sexprAsType _ (Atom (F64A x)) = pure x
  sexprAsType _ s               = throwError $ CannotGetAsType F64T s

instance AsLispType 'SymbolT where
  type HaskellType 'SymbolT = Text
  singLispType = SSymbolT
  sexprAsType _ (Atom (Symbol x)) = pure x
  sexprAsType _ s                 = throwError $ CannotGetAsType SymbolT s

instance AsLispType 'BoolT where
  type HaskellType 'BoolT = Bool
  singLispType = SBoolT
  sexprAsType _ (Atom (BoolA x)) = pure x
  sexprAsType _ s                = throwError $ CannotGetAsType BoolT s

instance AsLispType 'NilT where
  type HaskellType 'NilT = ()
  singLispType = SNilT
  sexprAsType _ (Atom SNil) = pure ()
  sexprAsType _ s           = throwError $ CannotGetAsType NilT s

data SExpr
    = Cons SExpr
           SExpr
    | Atom Atom
      deriving (Eq,Show)

pattern SFunction :: Text -> SExpr -> SExpr
pattern SFunction name args = Cons (Atom (Symbol name)) args

consList :: [SExpr] -> SExpr
consList []     = Atom SNil
consList (a:as) = Cons a (consList as)

expandConsCells :: SExpr -> [SExpr]
expandConsCells =
  let expandConsCellsHelper (Cons a b) = a : expandConsCells b
      expandConsCellsHelper s          = [s]
  in filter (/= Atom SNil) . expandConsCellsHelper

data EvalError
    = ParseError (ParseError Char Void)
    | CouldNotFindFunction Text
    | CouldNotFindVal Text
    | CannotGetAsType LispType SExpr
    | UnknownTypeError
    deriving (Eq, Show)

data LispFunction = LispFunction Bool [T.Text] SExpr
  deriving (Eq,Show)

data LispState = LispState
    { _lispFunctions :: M.Map T.Text LispFunction
    , _lispVals      :: [M.Map T.Text SExpr]
    } deriving (Eq,Show)

makeLenses ''LispState

getLispVal :: LispState -> Text -> Maybe SExpr
getLispVal ls t = ls ^? lispVals . traverse . ix t

instance Semigroup LispState where
  LispState f1 v1 <> LispState f2 v2 = LispState (f1 <> f2) (v1 <> v2)

instance Monoid LispState where
  mappend = (<>)
  mempty = LispState mempty mempty
