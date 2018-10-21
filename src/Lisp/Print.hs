{-# LANGUAGE OverloadedStrings #-}

module Lisp.Print where

import           Types

import           Data.Text (Text)
import qualified Data.Text as T

printAtom :: Atom -> Text
printAtom (F64A n)      = tShow n
printAtom (Symbol t)    = t
printAtom (BoolA True)  = "True"
printAtom (BoolA False) = "False"
printAtom SNil          = ""

printSExpr :: SExpr -> Text
printSExpr (Atom a) = printAtom a
printSExpr (Cons a b) =
    "(" <> T.init (T.intercalate " " (map printSExpr $ expandConsCells $ Cons a b)) <>
    ")"