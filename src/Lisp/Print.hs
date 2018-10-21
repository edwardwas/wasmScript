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

printType :: LispType -> Text
printType F64T            = "f64"
printType I64T            = "i64"
printType SymbolT         = "Symbol"
printType BoolT           = "Bool"
printType NilT            = "()"
printType (UnionT s)      = T.intercalate " U " (printType <$> S.toList s)
printType (FunctionT a b) = printType a <> " -> " <> printType b
