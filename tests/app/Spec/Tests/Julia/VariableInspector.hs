{-# LANGUAGE RankNTypes #-}

module Spec.Tests.Julia.VariableInspector (tests) where

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
                                  v = [1, 2, 3]
                                  m = [1 2; 3 4]
                                 |] $ \vars -> do
      filter (not . (`M.member` vars)) ["x", "s", "v", "m"] `shouldBe` []

      (variableInfoType <$> M.lookup "x" vars) `shouldBe` Just "Int64"
      (variableInfoIsMatrix <$> M.lookup "x" vars) `shouldBe` Just False

      (variableInfoType <$> M.lookup "s" vars) `shouldBe` Just "String"

      (variableInfoShape <$> M.lookup "v" vars) `shouldBe` Just (Just [3])

      (variableInfoShape <$> M.lookup "m" vars) `shouldBe` Just (Just [2, 2])
      (variableInfoIsMatrix <$> M.lookup "m" vars) `shouldBe` Just True

  it "hides its own module from the listing" $
    withListVariables kernel [i|x = 1|] $ \vars ->
      M.member "CodedownVariableInspector" vars `shouldBe` False

  it "inspects a scalar with no table" $
    withInspectVariable kernel [i|x = 42|] "x" $ \detail -> do
      variableDetailType detail `shouldBe` Just "Int64"
      variableDetailIsMatrix detail `shouldBe` False
      variableDetailTable detail `shouldBe` Nothing

  it "inspects a matrix with a populated table" $
    withInspectVariable kernel [i|m = [1 2; 3 4]|] "m" $ \detail -> do
      variableDetailIsMatrix detail `shouldBe` True
      (variableTableColumns <$> variableDetailTable detail) `shouldBe` Just ["1", "2"]
      (length . variableTableData <$> variableDetailTable detail) `shouldBe` Just 2
