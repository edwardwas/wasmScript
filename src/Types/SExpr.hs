{-# LANGUAGE PatternSynonyms #-}

module Types.SExpr where

import Data.Text (Text)

data Atom
    = FloatA Double
    | IntegerA Integer
    | Symbol Text
    | BoolA Bool
    | SNil
    deriving (Eq, Show)

data SExpr
    = Cons SExpr
           SExpr
    | Atom Atom
    deriving (Eq, Show)

pattern SFunction :: Text -> SExpr -> SExpr

pattern SFunction name args = Cons (Atom (Symbol name)) args

consList :: [SExpr] -> SExpr
consList [] = Atom SNil
consList (a:as) = Cons a (consList as)

expandConsCells :: SExpr -> [SExpr]
expandConsCells =
    let expandConsCellsHelper (Cons a b) = a : expandConsCells b
        expandConsCellsHelper s = [s]
     in filter (/= Atom SNil) . expandConsCellsHelper

data LispFunction =
    LispFunction Bool
                 [Text]
                 SExpr
    deriving (Eq, Show)
