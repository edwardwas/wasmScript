{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Lisp.Types where

import Types.Builtins
import Types.SExpr

import Control.Applicative
import Control.Lens hiding (Context)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Either
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

data AtomType
    = FloatT
    | IntegerT
    | BoolT
    | SymbolT
    | NilT
    deriving (Eq, Show, Ord)

typeOfAtom :: Atom -> AtomType
typeOfAtom (FloatA _) = FloatT
typeOfAtom (Symbol _) = SymbolT
typeOfAtom (BoolA _) = BoolT
typeOfAtom (SNil) = NilT

data LispType
    = AtomType AtomType
    | UnionType (Set LispType)
    | FunctionType LispType
                   LispType
    | AnyType
    deriving (Eq, Show, Ord)

floatT = AtomType FloatT

integerT = AtomType IntegerT

boolT = AtomType BoolT

data TypeError
    = CannotUnify LispType
                  LispType
    | CannotApply LispType
                  LispType
    deriving (Eq, Show)

applyType ::
       (Alternative m, MonadError [TypeError] m)
    => LispType
    -> LispType
    -> m LispType
applyType (FunctionType i o) a = o <$ unifyType i a
applyType (UnionType ts) a =
    foldr (<|>) empty $ map (\t -> applyType t a) $ S.toList ts
applyType a (UnionType ts) =
    foldr (<|>) empty $ map (\t -> applyType t a) $ S.toList ts
applyType a b = throwError [CannotApply a b]

applyWithArgs ::
       (MonadError [TypeError] m, Alternative m)
    => LispType
    -> [LispType]
    -> m LispType
applyWithArgs t [] = pure t
applyWithArgs t (a:as) = applyType t a >>= \t' -> applyWithArgs t' as

canUnify :: LispType -> LispType -> Bool
canUnify a b = isRight $ runExcept (unifyType a b)

canApplWithArgs :: LispType -> [LispType] -> Bool
canApplWithArgs a as = isRight $ runExcept (applyWithArgs a as)

unifyType ::
       (Alternative m, MonadError [TypeError] m)
    => LispType
    -> LispType
    -> m LispType
unifyType AnyType a = pure a
unifyType a AnyType = pure a
unifyType (AtomType a) (AtomType b)
    | a == b = pure $ AtomType a
    | otherwise = throwError [CannotUnify (AtomType a) (AtomType b)]
unifyType (UnionType as) b =
    foldr (<|>) empty $ map (\a -> unifyType a b) $ S.toList as
unifyType a (UnionType bs) =
    foldr (<|>) empty $ map (\b -> unifyType a b) $ S.toList bs
unifyType (FunctionType ia oa) (FunctionType ib ob) =
    FunctionType <$> (unifyType ia ib) <*> (unifyType oa ob)
unifyType a b = throwError [CannotUnify a b]
