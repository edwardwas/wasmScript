{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

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

allIdents :: [Text]
allIdents =
  let letters = ['a' .. 'z']
      helper = map pure letters ++ concatMap (\c -> map (c:) helper) letters
  in map (mappend "_" . T.pack) helper

class Monad m => MonadFreshIdent m where
  freshIdent :: m TypeVariable

type TypeVariable = Text

data Type
  = TVar TypeVariable
  | TAtom AtomType
  | TFuncCreate Type
                Type
  | TFuncApp Type
             Type
  deriving (Eq, Show, Ord)
makeBaseFunctor ''Type

data TypeScheme =
  TypeScheme (Set TypeVariable)
             Type
  deriving (Eq, Show, Ord)

data TypeConstraint = TCEquality Type Type

type Substition = Map TypeVariable Type
type Environment = Map TypeVariable TypeScheme

removeTypeFromEnvironemnt :: TypeVariable -> Environment -> Environment
removeTypeFromEnvironemnt = M.delete

freeTypeVars :: Type -> Set TypeVariable
freeTypeVars =
  let helper (TVarF t)          = S.singleton t
      helper (TAtomF _)         = S.empty
      helper (TFuncAppF a b)    = a `S.union` b
      helper (TFuncCreateF a b) = a `S.union` b
  in cata helper

freeTypeVarsScheme :: TypeScheme -> Set TypeVariable
freeTypeVarsScheme (TypeScheme s t) = freeTypeVars t S.\\ s

generalise :: Environment -> Type -> TypeScheme
generalise env ty = TypeScheme (freeTypeVars ty S.\\ foldMap freeTypeVarsScheme env) ty

applySubstition :: Substition -> Type -> Type
applySubstition m =
  let helper (TVarF x) = fromMaybe (TVar x) $ M.lookup x m
      helper t         = embed t
  in cata helper

instantiate :: MonadFreshIdent m => TypeScheme -> m Type
instantiate (TypeScheme as t) =
  (\as' -> applySubstition (M.fromList as') t) <$>
  mapM (\a -> (a, ) . TVar <$> freshIdent) (S.toList as)

numTy = TAtom FloatT
idTy = TFuncCreate (TVar "a") (TVar "a")
exampleTy = TFuncApp idTy numTy

