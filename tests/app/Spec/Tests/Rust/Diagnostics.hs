
module Spec.Tests.Rust.Diagnostics where

import Data.String.Interpolate
import Language.LSP.Types
import Test.Sandwich as Sandwich
import TestLib.LSP


diagnosticsTests :: (LspContext context m) => SpecFree context m ()
diagnosticsTests = describe "Diagnostics" $ do
  testDiagnostics "rust-analyzer" "main.ipynb" Nothing [__i|printlnz!("Hello world");
                                                           |] $ \diagnostics -> do
    assertDiagnosticRanges' diagnostics [
      (Range (Position 0 0) (Position 0 8), Nothing, "cannot find macro `printlnz` in this scope")
      , (Range (Position 0 0) (Position 0 8), Nothing, "a macro with a similar name exists: `println`")
      ]

  testDiagnostics "rust-analyzer" "test.rs" Nothing [__i|printlnz!("Hello world");
                                                         eprintln!("Hello error");
                                                         format!("Hello {}", "world")
                                                        |] $ \diagnostics -> do
    info [i|Got diagnostics: #{diagnostics}|]
    return ()
