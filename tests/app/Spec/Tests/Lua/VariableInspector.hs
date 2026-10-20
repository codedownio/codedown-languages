{-# LANGUAGE RankNTypes #-}

module Spec.Tests.Lua.VariableInspector (tests) where

import Data.String.Interpolate
import qualified Data.Map as M
import Data.Text (Text)
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.VariableInspector


tests :: (HasJupyterRunnerContext context, JupyterRunnerMonad m) => Text -> SpecFree context m ()
tests kernel = describe "Variable Inspector" $ do
  it "lists variables with types, sizes, and previews" $
    withListVariables kernel [__i|myNum = 42
                                  myStr = "hello"
                                  myArr = {1, 2, 3}
                                  myTab = {a = 1, b = "two"}
                                 |] $ \vars -> do
      filter (not . (`M.member` vars)) ["myNum", "myStr", "myArr", "myTab"] `shouldBe` []

      (variableInfoType <$> M.lookup "myNum" vars) `shouldBe` Just "number"
      (variableInfoContent <$> M.lookup "myNum" vars) `shouldBe` Just "42"
      (variableInfoIsMatrix <$> M.lookup "myNum" vars) `shouldBe` Just False

      (variableInfoType <$> M.lookup "myStr" vars) `shouldBe` Just "string"
      (variableInfoSize <$> M.lookup "myStr" vars) `shouldBe` Just (Just 5)

      -- A table whose keys are exactly 1..n is reported as an array, with a shape.
      (variableInfoType <$> M.lookup "myArr" vars) `shouldBe` Just "array"
      (variableInfoShape <$> M.lookup "myArr" vars) `shouldBe` Just (Just [3])
      (variableInfoIsMatrix <$> M.lookup "myArr" vars) `shouldBe` Just True

      (variableInfoType <$> M.lookup "myTab" vars) `shouldBe` Just "table"
      (variableInfoShape <$> M.lookup "myTab" vars) `shouldBe` Just Nothing
      (variableInfoIsMatrix <$> M.lookup "myTab" vars) `shouldBe` Just False

  -- Everything a Lua user assigns lands in _G next to the standard library and the
  -- globals xeus-lua injects, so the inspector lists only what appeared after its
  -- own baseline snapshot.
  it "hides the standard library, the kernel's globals, and its own helpers" $
    withListVariables kernel [i|x = 1|] $ \vars -> do
      filter (`M.member` vars) ["string", "table", "math", "ilua", "pprint"] `shouldBe` []
      M.member "__codedown_vi" vars `shouldBe` False
      M.member "__codedown_variable_inspector_list" vars `shouldBe` False

  it "inspects a scalar with no table" $
    withInspectVariable kernel [i|x = 42|] "x" $ \detail -> do
      variableDetailType detail `shouldBe` Just "number"
      variableDetailIsMatrix detail `shouldBe` False
      variableDetailTable detail `shouldBe` Nothing

  it "inspects an array with a one-column table" $
    withInspectVariable kernel [i|m = {10, 20, 30}|] "m" $ \detail -> do
      variableDetailIsMatrix detail `shouldBe` True
      variableDetailShape detail `shouldBe` Just [3]
      (variableTableColumns <$> variableDetailTable detail) `shouldBe` Just ["value"]
      (length . variableTableData <$> variableDetailTable detail) `shouldBe` Just 3

  it "inspects a keyed table as key/value rows" $
    withInspectVariable kernel [i|t = {a = 1, b = 2}|] "t" $ \detail -> do
      variableDetailType detail `shouldBe` Just "table"
      variableDetailIsMatrix detail `shouldBe` False
      (variableTableColumns <$> variableDetailTable detail) `shouldBe` Just ["key", "value"]
      (length . variableTableData <$> variableDetailTable detail) `shouldBe` Just 2

  it "reports an undefined variable rather than failing" $
    withInspectVariable kernel [i|x = 1|] "noSuchVariable" $ \detail -> do
      variableDetailContent detail `shouldBe` "<undefined>"
      variableDetailTable detail `shouldBe` Nothing
