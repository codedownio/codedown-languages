{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.R (tests) where

import Data.String.Interpolate
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers


kernelSpec :: NixKernelSpec
kernelSpec = NixKernelSpec {
  nixKernelName = "R"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "R"
  , nixKernelPackages = [nameOnly "ggplot2"]
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Nothing
  }

tests :: TopSpec
tests = describe "R" $ introduceNixEnvironment [kernelSpec] [] "R" $ introduceJupyterRunner $ do
  testKernelSearchersNonempty "R"

  testKernelStdout "R" [__i|cat("hi")|] "hi"
  testKernelStdout "R" [__i|print("hi")|] [i|[1] "hi"\n|]


main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
