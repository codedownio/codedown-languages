{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Go (tests) where

import Data.String.Interpolate
import Data.Text as T
import Language.LSP.Protocol.Types
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types

import qualified Spec.Tests.Go.Completion as Completion
import qualified Spec.Tests.Go.Hovers as Hovers


tests :: LanguageSpec
tests = describe "Go" $ do
  testKernelSearchersBuild "go"
  testHasExpectedFields "go"

  introduceNixEnvironment [kernelSpecWithLsp] [] "Go" $ introduceJupyterRunner $ do
    describe "Kernel tests" $ do
      testKernelStdout "go" [__i|import("fmt")
                                 fmt.Println("hi")|] "hi\n"

    describe "LSP" $ do
      testDiagnosticsLabel "gopls: Undeclared name" lsName "test.go" LanguageKind_Go printUnknownCode $ \diagnostics ->
        assertDiagnosticRanges diagnostics [(Range (Position 3 12) (Position 3 15), Just (InR "UndeclaredName"))]

      Completion.tests

      Hovers.tests

      itFormatsAs lsName "test.go" LanguageKind_Go badlyFormattedCode formattedCode

      itFormatsAsLabel "gopls formats main.ipynb" lsName "main.ipynb" LanguageKind_Go
        badlyFormattedCodeNotebook formattedCodeNotebook

      it "gopls formats main.ipynb with an import" $ do
        -- go-notebook-language-server leaves the import where it is when it projects for a
        -- formatting request, so gofmt gets `import ("fmt")` in statement position and
        -- refuses to parse: `expected statement, found 'import'`. See
        -- plans/notebook-formatting-in-proxies.md.
        _ <- pending
        formatsAs defaultTimeout lsName "main.ipynb" LanguageKind_Go
          badlyFormattedImportCodeNotebook formattedImportCodeNotebook

lsName :: Text
lsName = "gopls"

kernelSpecWithLsp :: NixKernelSpec
kernelSpecWithLsp = NixKernelSpec {
  nixKernelName = "go"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "Go"
  , nixKernelPackages = []
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelExtraConfig = Just [
      "lsp.gopls.enable = true"
      , "lsp.gopls.debug = true"
      ]
  }

badlyFormattedCode :: Text
badlyFormattedCode = "package main\nimport (\"fmt\")\nfunc main() {\nx:=1+2\nfmt.Println(x)\n}\n"

formattedCode :: Text
formattedCode = "package main\n\nimport (\n\t\"fmt\"\n)\n\nfunc main() {\n\tx := 1 + 2\n\tfmt.Println(x)\n}\n"

badlyFormattedCodeNotebook :: Text
badlyFormattedCodeNotebook = "x:=1+2\nfmt.Println(x)\n"

formattedCodeNotebook :: Text
formattedCodeNotebook = "x := 1 + 2\nfmt.Println(x)\n"

-- An import in a cell is the harder case: go-notebook-language-server sifts declarations out
-- of the wrapper function, so the projected document is a line permutation of the cell.
badlyFormattedImportCodeNotebook :: Text
badlyFormattedImportCodeNotebook = "import (\"fmt\")\nx:=1+2\nfmt.Println(x)\n"

formattedImportCodeNotebook :: Text
formattedImportCodeNotebook = "import (\"fmt\")\nx := 1 + 2\nfmt.Println(x)\n"

printUnknownCode :: Text
printUnknownCode = [__i|package main
                        import ("fmt")
                        func main() {
                        fmt.Println(foo)
                        }|]

main :: IO ()
main = jupyterMain tests
