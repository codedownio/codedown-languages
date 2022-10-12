
module Spec.Tests.Bash (tests) where

import Data.String.Interpolate
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.NixEnvironmentContext
import TestLib.NixTypes


kernelSpec = NixKernelSpec {
  nixKernelChannel = "codedown"
  , nixKernelName = "bashInteractive"
  , nixKernelDisplayName = Just "Bash"
  , nixKernelPackages = []
  , nixKernelLanguageServers = []
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Nothing
  }

tests :: TopSpec
tests = describe "Bash" $ introduceNixEnvironment [kernelSpec] [] "Bash" $ introduceJupyterRunner $ do
  testKernelStdout "bash" [i|echo hi|] "hi\n"



main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
