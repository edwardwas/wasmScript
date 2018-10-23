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
  | TFunction Type Type
    deriving (Eq,Show,Ord)
makeBaseFunctor ''Type

data TypeScheme =
  TypeScheme (Set TypeVariable)
             Type
  deriving (Eq, Show, Ord)

type Substition = Map TypeVariable Type

newtype Environment = Environment (Map TypeVariable TypeScheme)
  deriving (Eq,Show,Semigroup,Monoid)

class Substitutable s where
    freeTypes :: s -> Set TypeVariable
    applySub :: Substition -> s -> s

instance Substitutable Type where
    freeTypes (TVar t)        = S.singleton t
    freeTypes (TAtom _)       = S.empty
    freeTypes (TFunction a b) = S.union (freeTypes a) (freeTypes b)
    applySub s (TVar t)        = fromMaybe (TVar t) (M.lookup t s)
    applySub _ (TAtom a)       = TAtom a
    applySub s (TFunction a b) = TFunction (applySub s a) (applySub s b)

instance Substitutable TypeScheme where
    freeTypes (TypeScheme as t) = freeTypes t S.\\ as
    applySub s (TypeScheme as t) = TypeScheme as $ applySub (foldr M.delete s as) t

instance Substitutable Environment where
    freeTypes (Environment m) = foldMap freeTypes m
    applySub s (Environment m) = Environment (applySub s <$> m)

removeFromEnvironment :: TypeVariable -> Environment -> Environment
removeFromEnvironment x (Environment m) = Environment $ M.delete x m

generalise :: Environment -> Type -> TypeScheme
generalise e t = TypeScheme (freeTypes t S.\\ freeTypes e) t

instantiate :: MonadFreshIdent m => TypeScheme -> m Type
instantiate (TypeScheme as t) =
    (\as' -> applySub (M.fromList as') t) <$>
    mapM (\a -> (a, ) . TVar <$> freshIdent) (S.toList as)

unification =
    let isVar (TVar _) = True
        isVar _        = False
        rule1 = map (\(t,x) -> if isVar t then (x,t) else (t,x))
        rule2 :: [(Type,Type)] -> [(Type,Type)]
        rule2 = filter (uncurry (/=))
     in undefined

data TypeExpresion
    = Type Type
    | TEApp TypeExpresion
            TypeExpresion
    | TELambda TypeExpresion
               TypeExpresion
      deriving (Eq,Show)
