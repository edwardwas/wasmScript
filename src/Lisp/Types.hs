{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Lisp.Types where

import qualified Data.Set    as S
import           Types.SExpr

data LispType
    = F64T
    | SymbolT
    | BoolT
    | NilT
    | FunctionT LispType LispType
    deriving (Eq, Show)

functionTypeFromList :: [LispType] -> LispType
functionTypeFromList [a]    = a
functionTypeFromList (a:as) = FunctionT a $ functionTypeFromList as

