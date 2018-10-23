{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lisp.Eval where

import           Lisp.Print
import           Lisp.Read
import           Lisp.TypeCheck
import           Lisp.Types
import           Types.Builtins
import           Types.Errors
import           Types.SExpr
import           Types.State
import           Util

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Maybe           (isJust)
import qualified Data.Set             as S
import           Data.Text            (Text)

data LispEvalFunc =
    LispEvalFunc (forall m. (MonadState LispState m, MonadError EvalError m) =>
                                [SExpr] -> m SExpr)

builtinFunctions :: Builtin (M.Map LispType LispEvalFunc)
builtinFunctions =
    Builtin
        { addition =
              [ ( FunctionType floatT (FunctionType floatT floatT)
                , LispEvalFunc $
                  fmap (Atom . FloatA . sum) . traverse (sexprAsType SFloatT))
              , ( FunctionType integerT (FunctionType integerT integerT)
                , LispEvalFunc $
                  fmap (Atom . IntegerA . sum) .
                  traverse (sexprAsType SIntegerT))
              ]
        , equality =
              [ ( FunctionType floatT $ FunctionType floatT boolT
                , LispEvalFunc $
                  fmap (Atom . BoolA . isJust . allEq) .
                  traverse (sexprAsType SFloatT))
              , ( FunctionType integerT $ FunctionType integerT boolT
                , LispEvalFunc $
                  fmap (Atom . BoolA . isJust . allEq) .
                  traverse (sexprAsType SIntegerT))
              , ( FunctionType boolT $ FunctionType boolT boolT
                , LispEvalFunc $
                  fmap (Atom . BoolA . isJust . allEq) .
                  traverse (sexprAsType SBoolT))
              ]
        , coerceToFloat =
              [ (FunctionType floatT floatT, LispEvalFunc $ pure . head)
              , ( FunctionType integerT floatT
                , LispEvalFunc $
                  fmap (Atom . FloatA . fromIntegral) .
                  sexprAsType SIntegerT . head)
              ]
        }

makeTypesFromFuctions :: Builtin (Map LispType x) -> Builtin LispType
makeTypesFromFuctions = fmap (UnionType . S.fromList . M.keys)

evalLisp ::
       ( MonadReader (Builtin (Map LispType LispEvalFunc)) m
       , MonadState LispState m
       , MonadError EvalError m
       )
    => SExpr
    -> m SExpr
evalLisp (Atom (Symbol s)) = do
  mVal <- gets $ \ls -> getLispVal ls s
  case mVal of
    Just x  -> return x
    Nothing -> throwError $ CouldNotFindVal s
evalLisp (Atom a) = pure (Atom a)
evalLisp (SFunction "def" args) =
  case expandConsCells args of
      [Atom (Symbol n), val] -> do
          v <- evalLisp val
          lispVals %= (M.singleton n v :)
          return $ Atom $ SNil
      _ -> throwError $ UnknownTypeError ""
evalLisp (SFunction name args) = do
    bf <- ask
    args' <- mapM evalLisp $ expandConsCells args
    argsT :: [LispType] <- runReaderT (mapM typeOfSExpr args') $
            makeTypesFromFuctions bf
    case builtinAccess bf name of
        Just m ->
            case filter (\(t, _) -> canApplWithArgs t argsT) $ M.toList m of
                [(_, (LispEvalFunc f))] -> f $ expandConsCells args
                _ ->
                    throwError $
                    UnknownTypeError
                        "Cannot find function. This should have been caught be the type checker"
        Nothing -> undefined
evalLisp other = error $ show other

typeOfSExprHelper ::
       (MonadState LispState m, MonadReader (Builtin LispType) m)
    => SExpr
    -> ExceptT EvalError m LispType
typeOfSExprHelper (Atom (Symbol s)) = do
    ls <- get
    case getLispVal ls s of
        Just sexpr -> typeOfSExprHelper sexpr
        Nothing    -> throwError $ CouldNotFindVal s
typeOfSExprHelper (Atom a) = pure $ AtomType $ typeOfAtom a
typeOfSExprHelper (SFunction "def" args) = case expandConsCells args of
  [Atom (Symbol n), val] -> do
      _ <- typeOfSExprHelper val
      lispVals %= (M.singleton n val :)
      return $ AtomType NilT
  _ -> throwError $ UnknownTypeError "Arity mismatch"
typeOfSExprHelper (SFunction name args) = do
    builtinTypes <- ask
    case builtinAccess builtinTypes name of
        Just t ->
            traverse typeOfSExprHelper (expandConsCells args) >>=
            withExceptT (UnknownTypeError . show) . applyWithArgs t
        Nothing -> undefined

typeOfSExpr ::
       (MonadReader (Builtin LispType) m, MonadError EvalError m, MonadState LispState m)
    => SExpr
    -> m LispType
typeOfSExpr sexpr = do
    et <- runExceptT $ typeOfSExprHelper sexpr
    case et of
        Right x -> return x
        Left e  -> throwError e

checkType :: Text -> Either EvalError [Text]
checkType t =
    runExcept $
    evalStateT
        (runReaderT
             (loadSExpr t >>= mapM (fmap printType . typeOfSExpr))
             (makeTypesFromFuctions builtinFunctions))
        mempty
