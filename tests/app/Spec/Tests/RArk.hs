{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.RArk (tests) where

import Data.String.Interpolate
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types

import qualified Spec.Tests.R.VariableInspector as VariableInspector


kernelSpec :: NixKernelSpec
kernelSpec = NixKernelSpec {
  nixKernelName = "R-ark"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "R (Ark)"
  , nixKernelPackages = [nameOnly "ggplot2"]
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelExtraConfig = Nothing
  }

tests :: LanguageSpec
tests = describe "R (Ark)" $ introduceNixEnvironment [kernelSpec] [] "R-ark" $ introduceJupyterRunner $ do
  testKernelSearchersNonempty "R-ark"
  testHasExpectedFields "R-ark"

  testKernelStdout "R-ark" [__i|cat("hi")|] "hi"
  testKernelStdout "R-ark" [__i|print("hi")|] [i|[1] "hi"\n|]

  VariableInspector.tests "R-ark"


main :: IO ()
main = jupyterMain tests
