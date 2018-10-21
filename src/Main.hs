{-# LANGUAGE LambdaCase #-}

module Main where

import           App.Compile
import           App.Eval
import           App.Repl

import           Options.Applicative

data AppOptions
    = ReplApp ReplAppOptions
    | EvalApp EvalAppOptions
    | CompileApp CompileAppOptions
    deriving (Eq, Show)

parseAppOptions :: Parser AppOptions
parseAppOptions =
    hsubparser
        (command
             "repl"
             (info
                  (ReplApp <$> parseReplAppOptions)
                  (fullDesc <> progDesc "Start a repl")) <>
         command
             "eval"
             (info
                  (EvalApp <$> parseEvalAppOptions)
                  (fullDesc <> progDesc "Eval files")) <>
         command
             "compile"
             (info
                  (CompileApp <$> parseCompileAppOptions)
                  (fullDesc <> progDesc "Compile files")))

main :: IO ()
main =
    let opts =
            info
                (parseAppOptions <**> helper)
                (fullDesc <> progDesc "WasmScript!")
     in execParser opts >>= \case
            ReplApp rao -> runReplApp rao
            EvalApp eao -> runEvalApp eao
            CompileApp cao -> runCompileApp cao
