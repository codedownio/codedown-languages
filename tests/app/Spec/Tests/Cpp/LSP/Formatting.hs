module Spec.Tests.Cpp.LSP.Formatting (tests) where

import Spec.Tests.Cpp.Common
import Language.LSP.Protocol.Types
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = do
  itFormatsAs lsName "test.cpp" LanguageKind_CPP
    "int main(){int x=1+2;return x;}\n"
    "int main() {\n  int x = 1 + 2;\n  return x;\n}\n"

  it "clangd formats main.ipynb" $ do
    -- cpp-notebook-language-server forwards textDocument/formatting with the notebook's
    -- URI rather than the shadow file's, so clangd answers InvalidParams "trying to format
    -- non-added document". See plans/notebook-formatting-in-proxies.md.
    _ <- pending
    formatsAs defaultTimeout lsName "main.ipynb" LanguageKind_CPP
      "int x=1+2;\nstd::cout<<x;\n"
      "int x = 1 + 2;\nstd::cout << x;\n"
