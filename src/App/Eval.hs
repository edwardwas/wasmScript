{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module App.Eval where

import           Lisp.Eval
import           Lisp.Print
import           Lisp.Read
import           Types

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Text.IO         as T
import           Options.Applicative

data EvalAppOptions = EvalAppOptions
    { evalFilesToLoad :: [FilePath]
    } deriving (Eq, Show)

parseEvalAppOptions :: Parser EvalAppOptions
parseEvalAppOptions =
    EvalAppOptions <$>
    many (strArgument (metavar "FILE" <> help "Files to eval"))

evalFile ::
       (MonadError EvalError m, MonadIO m)
    => LispState
    -> FilePath
    -> m LispState
evalFile lispState fp =
    execStateT
        (liftIO (T.readFile fp) >>= loadSExpr >>= mapM evalLisp >>=
         liftIO . mapM (T.putStrLn . printSExpr))
        lispState

runEvalApp :: EvalAppOptions -> IO ()
runEvalApp EvalAppOptions{..} = do
  eRes <- runExceptT $ foldM evalFile mempty evalFilesToLoad
  case eRes of
    Right _ -> return ()
    Left e  -> putStrLn "There has been an error" >> print e
