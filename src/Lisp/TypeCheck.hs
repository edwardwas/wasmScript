{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Lisp.TypeCheck where

import           Lisp.Print           (printType)
import           Lisp.Read
import           Lisp.Types
import           Types.Builtins
import           Types.Errors
import           Types.SExpr
import           Types.State

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Map             (Map)
import qualified Data.Map             as M
import qualified Data.Set             as S
import           Data.Text            (Text)
import qualified Data.Text            as T
