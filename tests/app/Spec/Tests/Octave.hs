{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Octave (tests) where

import Data.String.Interpolate
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types


kernelSpec :: NixKernelSpec
kernelSpec = NixKernelSpec {
  nixKernelName = "octave"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "Octave"
  , nixKernelPackages = []
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelExtraConfig = Nothing
  }

tests :: LanguageSpec
tests = describe "Octave" $ introduceNixEnvironment [kernelSpec] [] "Octave" $ introduceJupyterRunner $ do
  testKernelSearchersBuild "octave"
  testHasExpectedFields "octave"

  testKernelStdout "octave" [__i|printf('%s', 'hi')|] "hi"


main :: IO ()
main = jupyterMain tests
