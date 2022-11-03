
module Spec.Tests.Julia (tests) where

import Data.String.Interpolate
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers


kernelSpec = NixKernelSpec {
  nixKernelName = "julia"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "Julia"
  , nixKernelPackages = []
  , nixKernelLanguageServers = []
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Nothing
  }

tests :: TopSpec
tests = describe "Julia" $ introduceNixEnvironment [kernelSpec] [] "Julia" $ introduceJupyterRunner $ do
  testKernelSearchers "julia"

  testKernelStdout "julia" [i|print("hi")|] "hi\n"


main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
