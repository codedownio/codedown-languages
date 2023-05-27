{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Rust (tests) where

import Data.Aeson as A
import Data.String.Interpolate
import Language.LSP.Types
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Util


tests :: TopSpec
tests = describe "Rust" $ introduceNixEnvironment [kernelSpec] [] "Rust" $ introduceJupyterRunner $ do
  testKernelSearchersBuild "rust"

  testKernelStdout "rust" [__i|println!("hi")|] "hi\n"

  -- testDiagnostics "rust-analyzer" "test.rs" Nothing [__i|struct A { a: u8, b: u8 }
  --                                                        let a = A { a: 10 };
  --                                                       |] $ \diagnostics -> do
  --   info [i|Got diagnostics: #{diagnostics}|]
  --   return ()

  -- testDiagnostics' "rust-analyzer" "src/main.rs" Nothing [__i|fn foo() {

  --                                                     }

  --                                                     fn main() {
  --                                                         println!("Hello, world!");
  --                                                     }
  --                                                    |] extraFiles $ \diagnostics -> do
  --   info [i|Got diagnostics: #{diagnostics}|]
  --   return ()

  testDiagnostics "rust-analyzer" "main.ipynb" Nothing [__i|println!("Hello world");
                                                            eprintln!("Hello error");
                                                            format!("Hello {}", "world")
                                                           |] $ \diagnostics -> do
    info [i|Got diagnostics: #{diagnostics}|]
    -- assertDiagnosticRanges diagnostics [
    --   (Range (Position 2 7) (Position 2 28), Nothing)
    --   ]
    return ()

  testDiagnostics "rust-analyzer" "test.rsz" Nothing [__i|printlnz!("Hello world");
                                                          eprintln!("Hello error");
                                                          format!("Hello {}", "world")
                                                         |] $ \diagnostics -> do
    info [i|Got diagnostics: #{diagnostics}|]
    return ()


kernelSpec :: NixKernelSpec
kernelSpec = NixKernelSpec {
  nixKernelName = "rust"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "Rust"
  , nixKernelPackages = []
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Just $ aesonFromList [
      ("lsp.rust-analyzer.debug", A.Bool True)
      ]
  }

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
