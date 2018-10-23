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

printAtomType :: AtomType -> Text
printAtomType FloatT   = "f64"
printAtomType IntegerT = "i64"
printAtomType SymbolT  = "Symbol"
printAtomType BoolT    = "Bool"
printAtomType NilT     = "()"

printType :: LispType -> Text
printType (AtomType a)       = printAtomType a
printType (UnionType a b)    = printType a <> " U " <> printType b
printType (FunctionType a b) = printType a <> " -> " <> printType b
printType AnyType            = "ANY"
