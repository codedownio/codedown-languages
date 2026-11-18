{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.R (tests) where

import Control.Monad (when)
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types

import qualified Spec.Tests.R.LSP as LSP
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

  when (backend == "ark") LSP.tests


main :: IO ()
main = jupyterMain tests
