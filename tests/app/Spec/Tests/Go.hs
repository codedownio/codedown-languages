
module Spec.Tests.Go (tests) where

import Data.String.Interpolate
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes


kernelSpec = NixKernelSpec {
  nixKernelName = "go"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "Go"
  , nixKernelPackages = []
  , nixKernelLanguageServers = [nameOnly "go-langserver"]
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Nothing
  }

tests :: TopSpec
tests = describe "Go" $ introduceNixEnvironment [kernelSpec] [] "Go" $ introduceJupyterRunner $ do
  testKernelStdout "go" [__i|import("fmt")
                             fmt.Println("hi")|] "hi\n"

  testDiagnostics "go-langserver" "test.go" [__i||] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics []

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
