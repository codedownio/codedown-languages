module Spec.Tests.Rust.Formatting (tests) where

import Language.LSP.Protocol.Types
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Formatting" $ do
  itFormatsAs' 300 "rust-analyzer" "test.rs" LanguageKind_Rust
    "fn main(){let x   =   1+2;println!(\"{}\",x);}\n"
    "fn main() {\n    let x = 1 + 2;\n    println!(\"{}\", x);\n}\n"

  -- A notebook cell goes through rust-notebook-language-server, which projects it into
  -- `fn main() { ... }` and has to undo that on the way back
  -- (rust-notebook-language-server#2).
  itFormatsAs' 300 "rust-analyzer" "main.ipynb" LanguageKind_Rust
    "let x   =   1+2;println!(\"{}\",x);\n"
    "let x = 1 + 2;\nprintln!(\"{}\", x);\n"
