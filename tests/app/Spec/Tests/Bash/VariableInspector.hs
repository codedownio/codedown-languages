{-# LANGUAGE RankNTypes #-}

module Spec.Tests.Bash.VariableInspector (tests) where

import Data.String.Interpolate
import qualified Data.Map as M
import Data.Text (Text)
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.VariableInspector


tests :: (HasJupyterRunnerContext context, JupyterRunnerMonad m) => Text -> SpecFree context m ()
tests kernel = describe "Variable Inspector" $ do
  it "lists user variables and not the inherited environment" $
    withListVariables kernel [__i|x=42
                                  s="hello world"
                                  arr=(a b c)
                                 |] $ \vars -> do
      filter (not . (`M.member` vars)) ["x", "s", "arr"] `shouldBe` []

      -- The inherited environment should not show up.
      M.member "PATH" vars `shouldBe` False
      M.member "HOME" vars `shouldBe` False

      (variableInfoType <$> M.lookup "x" vars) `shouldBe` Just "scalar"
      (variableInfoContent <$> M.lookup "x" vars) `shouldBe` Just "42"
      (variableInfoIsMatrix <$> M.lookup "x" vars) `shouldBe` Just False

      (variableInfoType <$> M.lookup "arr" vars) `shouldBe` Just "array"
      (variableInfoShape <$> M.lookup "arr" vars) `shouldBe` Just (Just [3])
      (variableInfoIsMatrix <$> M.lookup "arr" vars) `shouldBe` Just True

  it "hides its own helpers from the listing" $
    withListVariables kernel [i|x=1|] $ \vars ->
      M.member "__codedown_vi_baseline" vars `shouldBe` False

  it "inspects a scalar with no table" $
    withInspectVariable kernel [i|x=42|] "x" $ \detail -> do
      variableDetailType detail `shouldBe` Just "scalar"
      variableDetailContent detail `shouldBe` "42"
      variableDetailIsMatrix detail `shouldBe` False
      variableDetailTable detail `shouldBe` Nothing

  it "inspects an array with a populated table" $
    withInspectVariable kernel [i|arr=(a b c)|] "arr" $ \detail -> do
      variableDetailIsMatrix detail `shouldBe` True
      (variableTableColumns <$> variableDetailTable detail) `shouldBe` Just ["value"]
      (length . variableTableData <$> variableDetailTable detail) `shouldBe` Just 3
