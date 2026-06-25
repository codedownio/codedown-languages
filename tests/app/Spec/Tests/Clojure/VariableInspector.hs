{-# LANGUAGE RankNTypes #-}

module Spec.Tests.Clojure.VariableInspector (tests) where

import Data.String.Interpolate
import qualified Data.Map as M
import Data.Text (Text)
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.VariableInspector


tests :: (HasJupyterRunnerContext context, JupyterRunnerMonad m) => Text -> SpecFree context m ()
tests kernel = describe "Variable Inspector" $ do
  it "lists def'd vars with types, shapes, and previews" $
    withListVariables kernel [__i|(def x 42)
                                  (def s "hello")
                                  (def v [1 2 3])
                                  (def m {:a 1 :b 2})
                                 |] $ \vars -> do
      filter (not . (`M.member` vars)) ["x", "s", "v", "m"] `shouldBe` []

      (variableInfoType <$> M.lookup "x" vars) `shouldBe` Just "java.lang.Long"
      (variableInfoIsMatrix <$> M.lookup "x" vars) `shouldBe` Just False

      (variableInfoType <$> M.lookup "s" vars) `shouldBe` Just "java.lang.String"

      (variableInfoType <$> M.lookup "v" vars) `shouldBe` Just "clojure.lang.PersistentVector"
      (variableInfoShape <$> M.lookup "v" vars) `shouldBe` Just (Just [3])
      (variableInfoIsMatrix <$> M.lookup "v" vars) `shouldBe` Just True

      (variableInfoShape <$> M.lookup "m" vars) `shouldBe` Just (Just [2])
      (variableInfoIsMatrix <$> M.lookup "m" vars) `shouldBe` Just True

  it "hides its own helpers from the listing" $
    withListVariables kernel [i|(def x 1)|] $ \vars ->
      M.member "codedown-variable-inspector-list" vars `shouldBe` False

  it "inspects a scalar with no table" $
    withInspectVariable kernel [i|(def x 42)|] "x" $ \detail -> do
      variableDetailType detail `shouldBe` Just "java.lang.Long"
      variableDetailIsMatrix detail `shouldBe` False
      variableDetailTable detail `shouldBe` Nothing

  it "inspects a vector with a populated table" $
    withInspectVariable kernel [i|(def v [1 2 3])|] "v" $ \detail -> do
      variableDetailIsMatrix detail `shouldBe` True
      (variableTableColumns <$> variableDetailTable detail) `shouldBe` Just ["value"]
      (length . variableTableData <$> variableDetailTable detail) `shouldBe` Just 3
