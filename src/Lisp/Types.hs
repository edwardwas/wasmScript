{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE TypeFamilies     #-}

module Lisp.Types where

import           Data.Set       (Set)
import qualified Data.Set       as S
import           Types.Builtins
import           Types.SExpr

data LispType
    = FloatT
    | IntegerT
    | SymbolT
    | BoolT
    | NilT
    | UnionT (Set LispType)
    | ListOfT LispType
    | FunctionT LispType LispType
    deriving (Eq, Show, Ord)

typeOfAtom :: Atom -> LispType
typeOfAtom (FloatA _) = FloatT
typeOfAtom (Symbol _) = SymbolT
typeOfAtom (BoolA _)  = BoolT
typeOfAtom (SNil)     = NilT

numberT :: LispType
numberT = UnionT [FloatT , IntegerT]

instance Semigroup LispType where
    UnionT s1 <> UnionT s2 = UnionT (S.union s1 s2)
    a <> UnionT s = UnionT $ S.insert a s
    UnionT s <> a = UnionT $ S.insert a s
    a <> b
        | a == b = a
        | otherwise = UnionT $ S.fromList [a, b]

functionTypeFromList :: [LispType] -> LispType
functionTypeFromList [a]    = a
functionTypeFromList (a:as) = FunctionT a $ functionTypeFromList as

builtinTypes :: Builtin LispType
builtinTypes =
    Builtin
        { addition = FunctionT numberT $ FunctionT numberT numberT
        , equality = FunctionT FloatT $ FunctionT FloatT BoolT
        , coerceToFloat = FunctionT numberT FloatT
        }
