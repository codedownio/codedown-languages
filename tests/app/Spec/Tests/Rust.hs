
module Spec.Tests.Rust (tests) where

import Data.String.Interpolate
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes


kernelSpec = NixKernelSpec {
  nixKernelChannel = "codedown"
  , nixKernelLanguage = "rust_1_52"
  , nixKernelDisplayName = Just "Rust"
  , nixKernelPackages = []
  , nixKernelLanguageServers = [nameOnly "rust-analyzer"]
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettingsSchema = Nothing
  , nixKernelSettings = Nothing
  }

tests :: TopSpec
tests = describe "Rust" $ introduceNixEnvironment [kernelSpec] [] "Rust" $ introduceJupyterRunner $ do
  testKernelStdout "rust" [__i|println!("hi");|] "hi\n"

  testDiagnostics "rust-analyzer" "test.rs" [i|\n\n\nfoo = 42|] $ \diagnostics -> do
    return ()


main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
