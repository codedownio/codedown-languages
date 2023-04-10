{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Octave (tests) where

import Data.String.Interpolate
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers


kernelSpec :: NixKernelSpec
kernelSpec = NixKernelSpec {
  nixKernelName = "octave"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "Octave"
  , nixKernelPackages = []
  , nixKernelLanguageServers = []
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Nothing
  }

tests :: TopSpec
tests = describe "Octave" $ introduceNixEnvironment [kernelSpec] [] "Octave" $ introduceJupyterRunner $ do
  testKernelSearchersNonempty "octave"

  testKernelStdout "octave" [__i|printf('%s', 'hi')|] "hi"


main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
