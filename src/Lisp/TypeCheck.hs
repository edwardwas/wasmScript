{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Lisp.TypeCheck where

import Lisp.Print (printType)
import Lisp.Read
import Lisp.Types
import Types.Builtins
import Types.Errors
import Types.SExpr
import Types.State

import Control.Monad.Except
import Control.Monad.Reader
