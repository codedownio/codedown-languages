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

  it "languageserver formats test.R" $ do
    -- styler produces the right answer here -- you can watch it come back over the wire in the
    -- session log -- but the R language server then never answers lsp-test's `shutdown`, and
    -- the session hangs instead of finishing. Driving the same sequence against the same
    -- server in the same sandbox by hand shuts down fine, so it's something about how
    -- lsp-test ends the session. Pending until that's sorted out; a hanging test is worse
    -- than no test.
    _ <- pending
    formatsAs 180 "languageserver" "test.R" LanguageKind_R
      "f <- function(a,b){\na+b\n}\n"
      "f <- function(a, b) {\n  a + b\n}\n"


main :: IO ()
main = jupyterMain tests
