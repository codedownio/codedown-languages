module Spec.Tests.Rust.Formatting (tests) where

import Language.LSP.Protocol.Types
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Formatting" $
  it "rust-analyzer formats with rustfmt" $
    formatsAs 300 "rust-analyzer" "test.rs" LanguageKind_Rust
      "fn main(){let x   =   1+2;println!(\"{}\",x);}\n"
      "fn main() {\n    let x = 1 + 2;\n    println!(\"{}\", x);\n}\n"
