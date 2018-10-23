{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module Lisp.Types where

import           Types.Builtins
import           Types.SExpr

import           Control.Applicative
import           Control.Lens             hiding (Context)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import           Data.List.NonEmpty       (NonEmpty)
import           Data.Map                 (Map)
import qualified Data.Map                 as M
import           Data.Maybe
import           Data.Set                 (Set)
import qualified Data.Set                 as S
import           Data.Text                (Text)
import qualified Data.Text                as T

data AtomType = FloatT | IntegerT | BoolT | SymbolT | NilT
  deriving (Eq,Show,Ord)

typeOfAtom :: Atom -> AtomType
typeOfAtom (FloatA _) = FloatT
typeOfAtom (Symbol _) = SymbolT
typeOfAtom (BoolA _)  = BoolT
typeOfAtom (SNil)     = NilT

data LispType
  = AtomType AtomType
  | UnionType LispType
              LispType
  | FunctionType LispType LispType
  | AnyType
  deriving (Eq, Show,Ord)

unionTypeFromList :: NonEmpty LispType -> LispType
unionTypeFromList = foldr1 UnionType

floatT = AtomType FloatT
integerT = AtomType IntegerT
boolT = AtomType BoolT

data TypeError
  = CannotUnify LispType
                LispType
  | CannotApply LispType
                LispType
    deriving (Eq,Show)

applyType :: (Alternative m, MonadError [TypeError] m) => LispType -> LispType -> m LispType
applyType (FunctionType i o) a = o <$ unifyType i a
applyType (UnionType f1 f2) a  = applyType f1 a <|> applyType f2 a
applyType a b                  = throwError [CannotApply a b]

applyWithArgs ::
     (MonadError [TypeError] m, Alternative m)
  => LispType
  -> [LispType]
  -> m LispType
applyWithArgs t []     = pure t
applyWithArgs t (a:as) = applyType t a >>= \t' -> applyWithArgs t' as

unifyType :: (Alternative m, MonadError [TypeError] m) => LispType -> LispType -> m LispType
unifyType AnyType a = pure a
unifyType a AnyType = pure a
unifyType (AtomType a) (AtomType b)
  | a == b = pure $ AtomType a
  | otherwise = throwError [CannotUnify (AtomType a) (AtomType b)]
unifyType (UnionType a b) c = unifyType a c <|> unifyType b c
unifyType a (UnionType b c) = unifyType a b <|> unifyType a c
unifyType (FunctionType ia oa) (FunctionType ib ob) =
  FunctionType <$> (unifyType ia ib) <*> (unifyType oa ob)
unifyType a b = throwError [CannotUnify a b]
