{-# LANGUAGE RankNTypes #-}

module Spec.Tests.Python.VariableInspector (tests) where

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
                                  lst = [1, 2, 3]
                                  import numpy as np
                                  m = np.zeros((2, 3))
                                 |] $ \vars -> do
      filter (not . (`M.member` vars)) ["x", "s", "lst", "m"] `shouldBe` []

      (variableInfoType <$> M.lookup "x" vars) `shouldBe` Just "int"
      (variableInfoShape <$> M.lookup "x" vars) `shouldBe` Just Nothing
      (variableInfoIsMatrix <$> M.lookup "x" vars) `shouldBe` Just False

      (variableInfoType <$> M.lookup "s" vars) `shouldBe` Just "str"

      (variableInfoShape <$> M.lookup "lst" vars) `shouldBe` Just (Just [3])
      (variableInfoIsMatrix <$> M.lookup "lst" vars) `shouldBe` Just True

      (variableInfoType <$> M.lookup "m" vars) `shouldBe` Just "ndarray"
      (variableInfoShape <$> M.lookup "m" vars) `shouldBe` Just (Just [2, 3])
      (variableInfoIsMatrix <$> M.lookup "m" vars) `shouldBe` Just True

  it "hides its own object from the listing" $
    withListVariables kernel [i|x = 1|] $ \vars ->
      M.member "_codedown_variable_inspector" vars `shouldBe` False

  it "inspects a scalar with no table" $
    withInspectVariable kernel [i|x = 42|] "x" $ \detail -> do
      variableDetailType detail `shouldBe` Just "int"
      variableDetailContent detail `shouldBe` "42"
      variableDetailIsMatrix detail `shouldBe` False
      variableDetailTable detail `shouldBe` Nothing

  it "inspects a matrix with a populated table" $
    withInspectVariable kernel [__i|import numpy as np
                                    m = np.array([[1, 2], [3, 4]])
                                   |] "m" $ \detail -> do
      variableDetailIsMatrix detail `shouldBe` True
      (variableTableColumns <$> variableDetailTable detail) `shouldBe` Just ["0", "1"]
      (length . variableTableData <$> variableDetailTable detail) `shouldBe` Just 2
