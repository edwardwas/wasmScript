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
    | CannotGetAsType AtomType SExpr
    | TypeMismatch AtomType AtomType
    | UnknownTypeError
    | ArityMismatch
    deriving (Eq, Show)

data SAtomType (t :: AtomType) where
  SFloatT :: SAtomType 'FloatT
  SSymbolT :: SAtomType 'SymbolT
  SBoolT :: SAtomType 'BoolT
  SNilT :: SAtomType 'NilT

class AsAtomType t where
    type family HaskellType t :: *
    singAtomType :: SAtomType t
    sexprAsType :: MonadError EvalError m => proxy t -> SExpr -> m (HaskellType t)

instance AsAtomType 'FloatT where
  type HaskellType 'FloatT = Double
  singAtomType = SFloatT
  sexprAsType _ (Atom (FloatA x)) = pure x
  sexprAsType _ s                 = throwError $ CannotGetAsType FloatT s

instance AsAtomType 'SymbolT where
  type HaskellType 'SymbolT = Text
  singAtomType = SSymbolT
  sexprAsType _ (Atom (Symbol x)) = pure x
  sexprAsType _ s                 = throwError $ CannotGetAsType SymbolT s

instance AsAtomType 'BoolT where
  type HaskellType 'BoolT = Bool
  singAtomType = SBoolT
  sexprAsType _ (Atom (BoolA x)) = pure x
  sexprAsType _ s                = throwError $ CannotGetAsType BoolT s

instance AsAtomType 'NilT where
  type HaskellType 'NilT = ()
  singAtomType = SNilT
  sexprAsType _ (Atom SNil) = pure ()
  sexprAsType _ s           = throwError $ CannotGetAsType NilT s
