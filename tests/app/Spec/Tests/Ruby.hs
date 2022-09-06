
module Spec.Tests.Ruby (tests) where

import Data.String.Interpolate
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes


kernelSpec = NixKernelSpec {
  nixKernelChannel = "codedown"
  , nixKernelLanguage = "ruby"
  , nixKernelDisplayName = Just "Ruby"
  , nixKernelPackages = []
  , nixKernelLanguageServers = []
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Nothing
  }

tests :: TopSpec
tests = describe "Ruby" $ introduceNixEnvironment [kernelSpec] [] "Ruby" $ introduceJupyterRunner $ do
  testKernelStdout "ruby" [__i|puts "hi"|] "hi\n"

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
