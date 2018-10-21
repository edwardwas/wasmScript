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
    = F64T
    | I64T
    | SymbolT
    | BoolT
    | NilT
    | UnionT (Set LispType)
    | ListOfT LispType
    | FunctionT LispType LispType
    deriving (Eq, Show, Ord)

typeOfAtom :: Atom -> LispType
typeOfAtom (F64A _)   = F64T
typeOfAtom (Symbol _) = SymbolT
typeOfAtom (BoolA _)  = BoolT
typeOfAtom (SNil)     = NilT

numberT = UnionT [F64T, I64T]

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
        , equality = FunctionT F64T $ FunctionT F64T BoolT
        }
