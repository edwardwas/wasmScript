{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Builtins where

import           Data.Text (Text)

data Builtin a = Builtin
    { addition :: a
    , equality :: a
    } deriving (Eq,Show,Functor)

builtinAccess :: Builtin a -> Text -> Maybe a
builtinAccess b "+" = Just $ addition b
builtinAccess b "=" = Just $ equality b
builtinAccess _ _   = Nothing
