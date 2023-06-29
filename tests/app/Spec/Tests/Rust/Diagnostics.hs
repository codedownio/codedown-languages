
module Spec.Tests.Rust.Diagnostics where

import Control.Lens
import Data.Function
import qualified Data.List as L
import Data.String.Interpolate
import Language.LSP.Protocol.Types
import qualified Language.LSP.Protocol.Lens as LSP
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

  testDiagnostics "rust-analyzer" "test.rs" Nothing [__i|struct A { a: u8, b: u8 }
                                                         const a: A = A { a: 10, };
                                                        |] $ \diagnostics -> do
    assertDiagnosticRanges' (L.sortBy (compare `on` (^. LSP.message)) diagnostics) [
      (Range (Position 1 6) (Position 1 7), Just (InR "incorrect-ident-case"), "Constant `a` should have UPPER_SNAKE_CASE name, e.g. `A`")
      , (Range (Position 1 13) (Position 1 14), Just (InR "E0063"), "missing field `b` in initializer of `A`\nmissing `b`")
      , (Range (Position 1 13) (Position 1 14), Just (InR "missing-fields"), "missing structure fields:\n- b\n")
      ]
