module Util where

import           Control.Monad (guard)

allEq :: Eq a => [a] -> Maybe a
allEq []     = Nothing
allEq [a]    = Just a
allEq (a:as) = guard (all (a ==) as) >> allEq as
