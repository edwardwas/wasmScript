{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Wasm.Eval where

import           Types
import           Util

import           Control.Monad.Except
import           Data.Maybe           (isJust)
import           Data.Text            (Text)
import qualified Data.Text            as T

type WasmEvalFunc = forall m . (MonadError EvalError m) => [Text] -> m Text

equality :: [Double] -> SExpr
equality as = Atom $ BoolA $ isJust $ allEq as

addition :: WasmEvalFunc
addition args =
    let helper [] = "f64.const 0"
        helper as = T.unlines (as ++ replicate (length as - 1) "f64.add")
     in pure $ helper args

multiplication :: WasmEvalFunc
multiplication [] = pure "f64.const 0"
multiplication as = pure $ T.unlines (as ++ replicate (length as - 1) "f64.mul")

negation :: WasmEvalFunc
negation [] = pure "f64.const 0"
negation (a:as) = do
    addText <- addition as
    return $ T.unlines [a, addText, "f64.sub"]

defineFunction :: Text -> [Text] -> Text -> Text
defineFunction funcName toReplace body =
    let params =
            T.intercalate " " $ map (\s -> "(param $" <> s <> " f64)") toReplace
     in T.unlines
            ["(func $" <> funcName <> " " <> params <> " (result f64)", body, ")"]

condition :: MonadError EvalError m => [SExpr] -> m Text
condition (a:as) = do
    let [checkSExpr, valSExpr] = expandConsCells a
    checkProg <- evalWasm checkSExpr
    valProg <- evalWasm valSExpr
    nextProg <- condition as
    return $ T.unlines [checkProg, "if (result f64)", valProg, "else", nextProg,"end"]
condition [] = pure "unreachable"

evalWasm :: MonadError EvalError m => SExpr -> m Text
evalWasm (SFunction name args) = case name of
  "+"   -> traverse evalWasm (expandConsCells args) >>= addition
  "-"   -> traverse evalWasm (expandConsCells args) >>= negation
  "*"   -> traverse evalWasm (expandConsCells args) >>= multiplication
  "=" -> do
      aText <- traverse evalWasm (expandConsCells args)
      return $ T.unlines (aText ++ ["f64.eq"])
  "cond" -> condition $ expandConsCells args
  "defn" -> case expandConsCells args of
    [funcName,toReplace,body] -> do
      funcName' <- sexprAsType SSymbolT funcName
      toReplace' <- traverse (sexprAsType SSymbolT) $ expandConsCells toReplace
      body' <- evalWasm body
      return $ defineFunction funcName' toReplace' body'
    other -> error $ show other
  _ -> do
      pushToStack <- traverse evalWasm $ expandConsCells args
      return $ (T.unlines pushToStack) <> "\ncall $" <> name
evalWasm (Atom (F64A n)) = pure ("f64.const " <> tShow n)
evalWasm (Atom (Symbol n)) = pure $ "get_local $" <> n
evalWasm (Atom (BoolA True)) = pure ("i32.const 1")
evalWasm (Atom (BoolA False)) = pure ("i32.const 0")
evalWasm other = error $ show other
