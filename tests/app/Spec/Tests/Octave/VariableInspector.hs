{-# LANGUAGE RankNTypes #-}

module Spec.Tests.Octave.VariableInspector (tests) where

import Data.String.Interpolate
import qualified Data.Map as M
import Data.Text (Text)
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.VariableInspector


tests :: (HasJupyterRunnerContext context, JupyterRunnerMonad m) => Text -> SpecFree context m ()
tests kernel = describe "Variable Inspector" $ do
  it "lists variables with types, shapes, and previews" $
    withListVariables kernel [__i|x = 42;
                                  s = 'hello';
                                  m = [1 2; 3 4];
                                 |] $ \vars -> do
      filter (not . (`M.member` vars)) ["x", "s", "m"] `shouldBe` []

      (variableInfoType <$> M.lookup "x" vars) `shouldBe` Just "double"
      (variableInfoShape <$> M.lookup "x" vars) `shouldBe` Just (Just [1, 1])
      (variableInfoIsMatrix <$> M.lookup "x" vars) `shouldBe` Just False

      (variableInfoType <$> M.lookup "s" vars) `shouldBe` Just "char"
      (variableInfoIsMatrix <$> M.lookup "s" vars) `shouldBe` Just False

      (variableInfoShape <$> M.lookup "m" vars) `shouldBe` Just (Just [2, 2])
      (variableInfoIsMatrix <$> M.lookup "m" vars) `shouldBe` Just True

  it "hides its own function from the listing" $
    withListVariables kernel [i|x = 1;|] $ \vars ->
      M.member "codedown_variable_inspector" vars `shouldBe` False

  it "inspects a scalar with no table" $
    withInspectVariable kernel [i|x = 42;|] "x" $ \detail -> do
      variableDetailType detail `shouldBe` Just "double"
      variableDetailIsMatrix detail `shouldBe` False
      variableDetailTable detail `shouldBe` Nothing

  it "inspects a matrix with a populated table" $
    withInspectVariable kernel [i|m = [1 2; 3 4];|] "m" $ \detail -> do
      variableDetailIsMatrix detail `shouldBe` True
      (variableTableColumns <$> variableDetailTable detail) `shouldBe` Just ["1", "2"]
      (length . variableTableData <$> variableDetailTable detail) `shouldBe` Just 2
