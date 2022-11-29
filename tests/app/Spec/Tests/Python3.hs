
module Spec.Tests.Python3 (tests) where

import Control.Lens
import Data.String.Interpolate
import Language.LSP.Types
import Language.LSP.Types.Lens
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers


kernelSpec = NixKernelSpec {
  nixKernelName = "python3"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "Python 3"
  , nixKernelPackages = [nameOnly "tensorflow"]
  , nixKernelLanguageServers = [
      -- nameOnly "python-language-server"
      nameOnly "python-lsp-server"
      , nameOnly "pylint"
      , nameOnly "pyright"
      , nameOnly "pycodestyle"
      ]
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Nothing
  }

tests :: TopSpec
tests = describe "Python 3" $ introduceNixEnvironment [kernelSpec] [] "Python 3" $ introduceJupyterRunner $ do
  testKernelSearchers "python3"

  testKernelStdout "python3" [i|print("hi")|] "hi\n"
  testKernelStdout "python3" [i|print(42)|] "42\n"

  testKernelStdout "python3" [i|import tensorflow|] ""

  testDiagnostics "python-lsp-server" "test.py" [i|\n\n\nfoo = 42|] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics []

  testDiagnostics "pylint" "test.py" [i|\n\n\nfoo = 42|] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics []

  testDiagnostics "pyright" "test.py" [__i|\# pyright: strict
                                           def f(x: int, y: str) -> None:
                                             z = 1.0
                                           f("asdf", 42)
                                          |] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics [
      (Range (Position 3 2) (Position 3 8), Just (InR "reportGeneralTypeIssues"))
      , (Range (Position 3 10) (Position 3 12), Just (InR "reportGeneralTypeIssues"))

      , (Range (Position 1 6) (Position 1 7), Nothing)
      , (Range (Position 1 14) (Position 1 15), Nothing)

      , (Range (Position 2 2) (Position 2 3), Nothing)
      , (Range (Position 2 2) (Position 2 3), Just (InR "reportUnusedVariable"))
      ]

  testDiagnostics "pycodestyle" "test.py" [__i|def f(x: int, y: str) -> None:
                                                 z = 1.0
                                               f("asdf", 42)
                                              |] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics []

    -- (Range (Position 3 8) (Position 3 8), Just (InR "W292"))
    -- (Range (Position 3 0) (Position 3 8), Just (InR "E303"))

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
