{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.R (tests) where

import Data.String.Interpolate
import Language.LSP.Protocol.Types
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types

import qualified Spec.Tests.R.VariableInspector as VariableInspector


kernelSpec :: NixKernelSpec
kernelSpec = NixKernelSpec {
  nixKernelName = "R"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "R"
  , nixKernelPackages = [nameOnly "ggplot2"]
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelExtraConfig = Nothing
  }

tests :: LanguageSpec
tests = describe "R" $ introduceNixEnvironment [kernelSpec] [] "R" $ introduceJupyterRunner $ do
  testKernelSearchersNonempty "R"
  testHasExpectedFields "R"

  testKernelStdout "R" [__i|cat("hi")|] "hi"
  testKernelStdout "R" [__i|print("hi")|] [i|[1] "hi"\n|]

  VariableInspector.tests "R"

  it "languageserver formats with styler" $ do
    -- The R language server can't start under the hermetic PATH these tests use: loading
    -- processx runs system("which uname"), and R's system() goes through popen, which needs
    -- a /bin/sh the sandbox doesn't have. styler itself works -- the capability probe, which
    -- runs outside the sandbox, formats fine.
    _ <- pending
    formatsAs 180 "languageserver" "test.R" LanguageKind_R
      "f <- function(a,b){\na+b\n}\n"
      "f <- function(a, b) {\n  a + b\n}\n"


main :: IO ()
main = jupyterMain tests
