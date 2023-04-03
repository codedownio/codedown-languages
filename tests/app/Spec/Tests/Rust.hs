
module Spec.Tests.Rust (tests) where

import Data.String.Interpolate
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers


kernelSpec = NixKernelSpec {
  nixKernelName = "rust"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "Rust"
  , nixKernelPackages = []
  , nixKernelLanguageServers = [nameOnly "rust-analyzer"]
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Nothing
  }

tests :: TopSpec
tests = describe "Rust" $ introduceNixEnvironment [kernelSpec] [] "Rust" $ introduceJupyterRunner $ do
  testKernelSearchers "rust"

  testKernelStdout "rust" [__i|println!("hi")|] "hi\n"

  -- testDiagnostics "rust-analyzer" "test.rs" [__i|struct A { a: u8, b: u8 }
  --                                                let a = A { a: 10 };
  --                                               |] $ \diagnostics -> do
  --   info [i|Got diagnostics: #{diagnostics}|]
  --   return ()

  testDiagnostics' "rust-analyzer" "src/test.rs" [__i|fn foo() {

                                                      }
                                                     |] extraFiles $ \diagnostics -> do
    info [i|Got diagnostics: #{diagnostics}|]
    return ()


extraFiles = [
  ("Cargo.toml", [__i|[package]
                      name = "rust_test"
                      version = "0.1.0"
                      edition = "2018"
                     |])
  , ("src/main.rs", [__i|fn main() {
                             println!("Hello, world!");
                         }
                        |])

  ]

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
