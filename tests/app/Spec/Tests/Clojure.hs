
module Spec.Tests.Clojure (tests) where

import Data.String.Interpolate
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes


kernelSpec = NixKernelSpec {
  nixKernelName = "clojure"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "Clojure"
  , nixKernelPackages = []
  , nixKernelLanguageServers = [nameOnly "clojure-lsp"]
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Nothing
  }

tests :: TopSpec
tests = describe "Clojure" $ introduceNixEnvironment [kernelSpec] [] "Clojure" $ introduceJupyterRunner $ do
  testKernelStdout "clojure" [__i|(println "hi")|] "hi\n"

  testDiagnostics "go-langserver" "test.clj" [__i||] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics []


main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
