{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}

module Types.Errors where

import           Lisp.Types
import           Types.SExpr
import           Util

import           Control.Monad.Except
import           Data.Text            (Text)
import           Data.Void
import           Text.Megaparsec      (ParseError)

data EvalError
    = ParseError (ParseError Char Void)
    | CouldNotFindFunction Text
    | CouldNotFindVal Text
    | CannotGetAsType LispType SExpr
    | TypeMismatch LispType LispType
    | UnknownTypeError
    | ArityMismatch
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
