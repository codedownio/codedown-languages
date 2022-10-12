
module Spec.Tests.Octave (tests) where

import Data.String.Interpolate
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.NixEnvironmentContext
import TestLib.NixTypes


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
  testKernelStdout "octave" [__i|printf('%s', 'hi')|] "hi"


main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
