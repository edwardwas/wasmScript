{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Repl where

import Lisp.Eval
import Lisp.Print
import Lisp.Read
import Lisp.Types
import Types.Builtins
import Types.Errors
import Types.SExpr
import Types.State

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
import System.Console.Readline

data ReplAppOptions = ReplAppOptions
    { replFilesToLoad :: [FilePath]
    , replHistoryFileLocation :: FilePath
    } deriving (Eq, Show)

parseReplAppOptions :: Parser ReplAppOptions
parseReplAppOptions =
    ReplAppOptions <$>
    many (strArgument (metavar "FILE" <> help "Files to load into the repl")) <*>
    strOption
        (value ".repl-history" <> help "Where to store the repl history" <>
         long "history-file")

singleRepl :: Monad m => LispState -> Text -> m ([Text], LispState)
singleRepl lispState inputText = do
    let helper bf = do
            startSExpr <- loadSExpr inputText
            (ty, _) <-
                runReaderT
                    (runStateT (mapM typeOfSExpr startSExpr) lispState)
                    (makeTypesFromFuctions bf)
            (endSExprs, newState) <-
                runReaderT (runStateT (mapM evalLisp startSExpr) lispState) bf
            return (ty, endSExprs, newState)
    resultSExpr <- runExceptT (helper builtinFunctions)
    return $
        case resultSExpr of
            Right (ty, sexprs, newLispState) ->
                (map printType ty ++ " " : map printSExpr sexprs, newLispState)
            Left e -> ([T.pack $ show e], lispState)

runRepl :: (String -> IO ()) -> LispState -> IO ()
runRepl recordLine lispState = do
    maybeLine <- readline "repl> "
    case maybeLine of
        Nothing -> return ()
        Just t -> do
            addHistory t
            recordLine t
            (toPrint, newLispState) <- singleRepl lispState $ T.pack t
            mapM_ T.putStrLn toPrint
            runRepl recordLine newLispState

loadFileLispState ::
       (MonadError EvalError m, MonadIO m)
    => LispState
    -> FilePath
    -> m LispState
loadFileLispState lispState fp =
    runReaderT
        (execStateT
             (liftIO (T.readFile fp) >>= loadSExpr >>= mapM evalLisp)
             lispState)
        builtinFunctions

runReplApp :: ReplAppOptions -> IO ()
runReplApp ReplAppOptions {..} = do
    eLispState <- runExceptT (foldM loadFileLispState mempty replFilesToLoad)
    T.readFile replHistoryFileLocation >>=
        mapM_ (addHistory . T.unpack) . T.lines
    case eLispState of
        Right lispState ->
            runRepl (appendFile replHistoryFileLocation . (<> "\n")) lispState
        Left e -> putStrLn "There has been an error" >> print e
