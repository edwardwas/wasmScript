{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module App.Compile where

import           Lisp.Read
import           Types
import           Wasm.Eval

import           Control.Monad.Except
import           Data.List            (intercalate)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Options.Applicative
import           Shelly               (run_, shelly)
import           System.FilePath

data CompileAppOptions = CompileAppOptions
    { compileInputFiles  :: [FilePath]
    , compileOutputFiles :: FilePath
    , compileToWasm      :: Bool
    } deriving (Eq,Show)

parseCompileAppOptions :: Parser CompileAppOptions
parseCompileAppOptions =
    CompileAppOptions <$>
    many (strArgument (metavar "FILE" <> help "Files to compile")) <*>
    strOption
        (value "out.wat" <> help "Output file location" <> short 'o' <>
         long "output-location") <*>
    switch
        (short 'w' <> long "compile-wasm" <>
         help "Compile the output wat file to wasm")

makeSingleWasm :: (MonadError EvalError m, MonadIO m) => FilePath -> [SExpr] -> m ()
makeSingleWasm ofile sexprs = do
    let ss = init sexprs
        s = last sexprs
    textToWriteFore <- T.unlines <$> mapM evalWasm ss
    textToWriteLast <- evalWasm s
    liftIO $
        T.writeFile ofile $
        T.unlines
            [ "(module"
            , textToWriteFore
            , "\t(func $start (result f64) "
            , textToWriteLast
            , ")"
            , "\t(export \"_main\" (func $start))"
            , ")"
            ]

loadFiles :: (MonadIO m, MonadError EvalError m) => [FilePath] -> m [SExpr]
loadFiles [] = return []
loadFiles (f:fs) = do
    here <- liftIO (T.readFile f) >>= loadSExpr
    there <- loadFiles fs
    return (here ++ there)

runCompileApp :: CompileAppOptions -> IO ()
runCompileApp CompileAppOptions {..} = do
    putStrLn $
        "Compiling " <> intercalate ", " compileInputFiles <> " to " <>
        compileOutputFiles
    eRes <-
        runExceptT
            (loadFiles compileInputFiles >>= makeSingleWasm compileOutputFiles)
    case eRes of
        Right _ ->
            when compileToWasm $ do
                putStrLn $ "Compiling " <> compileOutputFiles <> " to " <> (dropExtension compileOutputFiles <.> "wasm")
                shelly $ run_ "wat2wasm" [T.pack compileOutputFiles]
        Left e -> putStrLn "There has been an error" >> print e
