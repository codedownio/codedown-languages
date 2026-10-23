module Spec.Tests.Go.LSP.Formatting (tests) where

import Data.Text (Text)
import Language.LSP.Protocol.Types
import Spec.Tests.Go.Common
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = do
  itFormatsAs lsName "test.go" LanguageKind_Go badlyFormattedCode formattedCode

  it "gopls formats main.ipynb" $ do
    -- go-notebook-language-server forwards textDocument/formatting with the notebook's URI
    -- rather than the shadow file's, so gopls answers "no file content for file://.../main.ipynb".
    -- See plans/notebook-formatting-in-proxies.md.
    _ <- pending
    formatsAs defaultTimeout lsName "main.ipynb" LanguageKind_Go
      badlyFormattedCodeNotebook formattedCodeNotebook

badlyFormattedCode :: Text
badlyFormattedCode = "package main\nimport (\"fmt\")\nfunc main() {\nx:=1+2\nfmt.Println(x)\n}\n"

formattedCode :: Text
formattedCode = "package main\n\nimport (\n\t\"fmt\"\n)\n\nfunc main() {\n\tx := 1 + 2\n\tfmt.Println(x)\n}\n"

badlyFormattedCodeNotebook :: Text
badlyFormattedCodeNotebook = "x:=1+2\nfmt.Println(x)\n"

formattedCodeNotebook :: Text
formattedCodeNotebook = "x := 1 + 2\nfmt.Println(x)\n"
