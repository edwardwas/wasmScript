module Util where

import Control.Monad (guard)
import Data.Text (Text)
import qualified Data.Text as T

allEq :: Eq a => [a] -> Maybe a
allEq [] = Nothing
allEq [a] = Just a
allEq (a:as) = guard (all (a ==) as) >> allEq as

tShow :: Show a => a -> Text
tShow = T.pack . show
