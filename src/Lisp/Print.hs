{-# LANGUAGE OverloadedStrings #-}

module Lisp.Print where

import           Lisp.Types
import           Types.Builtins
import           Types.SExpr
import           Util

import qualified Data.Set       as S
import           Data.Text      (Text)
import qualified Data.Text      as T

printAtom :: Atom -> Text
printAtom (FloatA n)    = tShow n
printAtom (Symbol t)    = t
printAtom (BoolA True)  = "True"
printAtom (BoolA False) = "False"
printAtom SNil          = ""

printSExpr :: SExpr -> Text
printSExpr (Atom a) = printAtom a
printSExpr (Cons a b) =
    "(" <> T.init (T.intercalate " " (map printSExpr $ expandConsCells $ Cons a b)) <>
    ")"

printType :: AtomType -> Text
printType FloatT   = "f64"
printType IntegerT = "i64"
printType SymbolT  = "Symbol"
printType BoolT    = "Bool"
printType NilT     = "()"
