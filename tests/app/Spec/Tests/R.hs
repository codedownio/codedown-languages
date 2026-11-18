{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.R (tests) where

import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import Language.LSP.Protocol.Types
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types

import qualified Spec.Tests.R.VariableInspector as VariableInspector


kernelSpec :: Text -> NixKernelSpec
kernelSpec backend = NixKernelSpec {
  nixKernelName = "R"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "R"
  , nixKernelPackages = [nameOnly "ggplot2"]
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelExtraConfig = Just [[i|backend = "#{backend}"|]]
  }

tests :: LanguageSpec
tests = describe "R" $ do
  backendTests "ark"
  backendTests "irkernel"

backendTests :: SomeLanguageSpec context => Text -> SpecFree context IO ()
backendTests backend = describe (T.unpack backend) $ introduceNixEnvironment [kernelSpec backend] [] [i|R (#{backend})|] $ introduceJupyterRunner $ do
  testKernelSearchersNonempty "R"
  testHasExpectedFields "R"

  testKernelStdout "R" [__i|cat("hi")|] "hi"
  testKernelStdout "R" [__i|print("hi")|] [i|[1] "hi"\n|]

  VariableInspector.tests "R"

  itFormatsAs "languageserver" "test.R" LanguageKind_R
    "f <- function(a,b){\na+b\n}\n"
    "f <- function(a, b) {\n  a + b\n}\n"


main :: IO ()
main = jupyterMain tests
