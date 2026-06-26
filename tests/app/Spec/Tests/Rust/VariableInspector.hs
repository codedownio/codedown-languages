{-# LANGUAGE RankNTypes #-}

module Spec.Tests.Rust.VariableInspector (tests) where

import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text (Text)
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.VariableInspector


-- evcxr has no in-code introspection, so the inspector uses a :vars_json command
-- (a codedown addition to evcxr; see ~/tools/evcxr) that yields variable names and
-- types as JSON (no values). Hence list-only.
tests :: (HasJupyterRunnerContext context, JupyterRunnerMonad m) => Text -> SpecFree context m ()
tests kernel = describe "Variable Inspector" $
  it "lists bound variables and their types via :vars_json" $ do
    -- Pending until codedown's evcxr build includes the :vars_json patch.
    pending
    withListVariables kernel [__i|let x: i32 = 42;
                                  let s: String = String::from("hi");
                                  let v: Vec<i32> = vec![1, 2, 3];|] $ \vars -> do
      filter (not . (`M.member` vars)) ["x", "s", "v"] `shouldBe` []
      (variableInfoType <$> M.lookup "x" vars) `shouldBe` Just "i32"
      (variableInfoType <$> M.lookup "s" vars) `shouldBe` Just "String"
      (variableInfoType <$> M.lookup "v" vars) `shouldBe` Just "Vec<i32>"
