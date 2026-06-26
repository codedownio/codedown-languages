{-# LANGUAGE RankNTypes #-}

module Spec.Tests.Ruby.VariableInspector (tests) where

import Data.String.Interpolate
import qualified Data.Map as M
import Data.Text (Text)
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.VariableInspector


tests :: (HasJupyterRunnerContext context, JupyterRunnerMonad m) => Text -> SpecFree context m ()
tests kernel = describe "Variable Inspector" $ do
  it "lists variables with types, shapes, and previews" $
    withListVariables kernel [__i|x = 42
                                  s = "hello"
                                  arr = [1, 2, 3]
                                  h = {a: 1, b: 2}
                                 |] $ \vars -> do
      filter (not . (`M.member` vars)) ["x", "s", "arr", "h"] `shouldBe` []

      (variableInfoType <$> M.lookup "x" vars) `shouldBe` Just "Integer"
      (variableInfoIsMatrix <$> M.lookup "x" vars) `shouldBe` Just False

      (variableInfoType <$> M.lookup "s" vars) `shouldBe` Just "String"

      (variableInfoType <$> M.lookup "arr" vars) `shouldBe` Just "Array"
      (variableInfoShape <$> M.lookup "arr" vars) `shouldBe` Just (Just [3])
      (variableInfoIsMatrix <$> M.lookup "arr" vars) `shouldBe` Just True
      (variableInfoContent <$> M.lookup "arr" vars) `shouldBe` Just "[1, 2, 3]"

  it "hides iruby's history variables and its own module" $
    withListVariables kernel [i|x = 1|] $ \vars -> do
      M.member "_" vars `shouldBe` False
      M.member "_i" vars `shouldBe` False

  it "inspects a scalar with no table" $
    withInspectVariable kernel [i|x = 42|] "x" $ \detail -> do
      variableDetailType detail `shouldBe` Just "Integer"
      variableDetailContent detail `shouldBe` "42"
      variableDetailIsMatrix detail `shouldBe` False
      variableDetailTable detail `shouldBe` Nothing

  it "inspects an array with a populated table" $
    withInspectVariable kernel [i|arr = [10, 20, 30]|] "arr" $ \detail -> do
      variableDetailIsMatrix detail `shouldBe` True
      (variableTableColumns <$> variableDetailTable detail) `shouldBe` Just ["value"]
      (length . variableTableData <$> variableDetailTable detail) `shouldBe` Just 3
