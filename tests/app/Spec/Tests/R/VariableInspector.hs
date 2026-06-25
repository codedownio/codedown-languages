{-# LANGUAGE RankNTypes #-}

module Spec.Tests.R.VariableInspector (tests) where

import Data.String.Interpolate
import qualified Data.Map as M
import Data.Text (Text)
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.VariableInspector


tests :: (HasJupyterRunnerContext context, JupyterRunnerMonad m) => Text -> SpecFree context m ()
tests kernel = describe "Variable Inspector" $ do
  it "lists variables with types, shapes, and previews" $
    withListVariables kernel [__i|x <- 42L
                                  s <- "hello"
                                  v <- c(1, 2, 3)
                                  m <- matrix(1:6, nrow = 2)
                                  df <- data.frame(a = 1:3, b = 4:6)
                                 |] $ \vars -> do
      filter (not . (`M.member` vars)) ["x", "s", "v", "m", "df"] `shouldBe` []

      (variableInfoType <$> M.lookup "x" vars) `shouldBe` Just "integer"
      (variableInfoIsMatrix <$> M.lookup "x" vars) `shouldBe` Just False

      (variableInfoType <$> M.lookup "s" vars) `shouldBe` Just "character"

      (variableInfoShape <$> M.lookup "v" vars) `shouldBe` Just (Just [3])
      (variableInfoIsMatrix <$> M.lookup "v" vars) `shouldBe` Just False

      (variableInfoShape <$> M.lookup "m" vars) `shouldBe` Just (Just [2, 3])
      (variableInfoIsMatrix <$> M.lookup "m" vars) `shouldBe` Just True

      (variableInfoType <$> M.lookup "df" vars) `shouldBe` Just "data.frame"
      (variableInfoShape <$> M.lookup "df" vars) `shouldBe` Just (Just [3, 2])
      (variableInfoIsMatrix <$> M.lookup "df" vars) `shouldBe` Just True

  it "hides its own object from the listing" $
    withListVariables kernel [i|x <- 1|] $ \vars ->
      M.member ".codedown_variable_inspector" vars `shouldBe` False

  it "inspects a scalar with no table" $
    withInspectVariable kernel [i|x <- 42L|] "x" $ \detail -> do
      variableDetailType detail `shouldBe` Just "integer"
      variableDetailIsMatrix detail `shouldBe` False
      variableDetailTable detail `shouldBe` Nothing

  it "inspects a data.frame with a populated table" $
    withInspectVariable kernel [i|df <- data.frame(a = 1:3, b = 4:6)|] "df" $ \detail -> do
      variableDetailIsMatrix detail `shouldBe` True
      (variableTableColumns <$> variableDetailTable detail) `shouldBe` Just ["a", "b"]
      (length . variableTableData <$> variableDetailTable detail) `shouldBe` Just 3
