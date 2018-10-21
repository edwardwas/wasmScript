{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Lisp.Read
    ( loadSExpr
    ) where

import           Types

import           Control.Monad.Except
import           Data.Bifunctor             (first)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = ParsecT Void Text (Either EvalError)

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexer :: Parser a -> Parser a
lexer = L.lexeme spaceConsumer

symbolHeadChars :: [Char]
symbolHeadChars = ['a' .. 'z'] ++ ['+', '-', '*', '/', '?', '=']

symbolOtherChars :: [Char]
symbolOtherChars = symbolHeadChars ++ ['A' .. 'Z'] ++ ['0' .. '9']

parseSymbol :: Parser Atom
parseSymbol =
    lexer $ do
        h <- oneOf symbolHeadChars
        t <- many (oneOf symbolOtherChars)
        return $ Symbol $ T.pack (h : t)

parseAtom :: Parser Atom
parseAtom =
    lexer $
    choice $
    map try
        [ BoolA True <$ "True"
        , BoolA False <$ "False"
        , F64A <$> L.float
        , F64A . fromIntegral <$> (L.decimal :: Parser Integer)
        , parseSymbol
        ]

parseSExpr :: Parser SExpr
parseSExpr =
    lexer $
    choice $
        [ Atom <$> parseAtom
        , between (lexer $ char '(') (lexer $ char ')') (consList <$> many parseSExpr)
        ]

loadSExpr :: MonadError EvalError m => Text -> m [SExpr]
loadSExpr t =
    case join (first ParseError <$> runParserT (many parseSExpr) "" t) of
        Right x -> return x
        Left e  -> throwError e
