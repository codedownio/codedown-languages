
module Spec.Tests.Octave (tests) where

import Data.String.Interpolate
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.NixEnvironmentContext
import TestLib.NixTypes


kernelSpec = NixKernelSpec {
  nixKernelChannel = "codedown"
  , nixKernelLanguage = "octave"
  , nixKernelDisplayName = Just "Octave"
  , nixKernelPackages = []
  , nixKernelLanguageServers = []
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettingsSchema = Nothing
  , nixKernelSettings = Nothing
  }

tests :: TopSpec
tests = describe "Octave" $ introduceNixEnvironment [kernelSpec] [] "Octave" $ introduceJupyterRunner $ do
  testKernelStdout "octave" [__i|disp("hi")|] "hi\n"


main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
