module Spec.Tests.Rust.Formatting (tests) where

import Language.LSP.Protocol.Types
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Formatting" $
  it "rust-analyzer formats with rustfmt" $ do
    -- rust-analyzer itself formats fine now that rustfmt is on its PATH, but
    -- rust-notebook-language-server forwards textDocument/formatting with the notebook's URI
    -- rather than the shadow file's, so rust-analyzer answers "file not found". Everything
    -- else is rewritten, which is why the rest of the LSP tests pass.
    --
    -- https://github.com/codedownio/rust-notebook-language-server/pull/2 fixes it; this test
    -- can come off pending once that's released and rnls-version.nix moves up.
    _ <- pending
    formatsAs 300 "rust-analyzer" "test.rs" LanguageKind_Rust
      "fn main(){let x   =   1+2;println!(\"{}\",x);}\n"
      "fn main() {\n    let x = 1 + 2;\n    println!(\"{}\", x);\n}\n"
