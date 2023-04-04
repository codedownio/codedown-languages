{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Python3 (tests) where

import Data.String.Interpolate
import Language.LSP.Types
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers


kernelSpec :: NixKernelSpec
kernelSpec = NixKernelSpec {
  nixKernelName = "python3"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "Python 3"
  , nixKernelPackages = [nameOnly "tensorflow"]
  , nixKernelLanguageServers = [
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
    assertDiagnosticRanges' diagnostics [
      (Range (Position 3 0) (Position 3 0), Nothing, "Final newline missing (C0304:missing-final-newline)")
      , (Range (Position 0 0) (Position 0 0), Nothing, "Missing module docstring (C0114:missing-module-docstring)")
      , (Range (Position 3 0) (Position 3 0), Nothing, "Disallowed name \"foo\" (C0104:disallowed-name)")
      ]

  testDiagnostics "pyright" "test.py" [__i|\# pyright: strict
                                           def f(x: int, y: str) -> None:
                                             z = 1.0
                                           f("asdf", 42)
                                          |] $ \diagnostics -> do
    assertDiagnosticRanges' diagnostics [
      (Range (Position 3 2) (Position 3 8), Just (InR "reportGeneralTypeIssues"), "Argument of type \"Literal['asdf']\" cannot be assigned to parameter \"x\" of type \"int\" in function \"f\"\n\160\160\"Literal['asdf']\" is incompatible with \"int\"")
      , (Range (Position 3 10) (Position 3 12), Just (InR "reportGeneralTypeIssues"), "Argument of type \"Literal[42]\" cannot be assigned to parameter \"y\" of type \"str\" in function \"f\"\n\160\160\"Literal[42]\" is incompatible with \"str\"")

      , (Range (Position 1 6) (Position 1 7), Nothing, "\"x\" is not accessed")
      , (Range (Position 1 14) (Position 1 15), Nothing, "\"y\" is not accessed")

      , (Range (Position 2 2) (Position 2 3), Nothing, "\"z\" is not accessed")
      , (Range (Position 2 2) (Position 2 3), Just (InR "reportUnusedVariable"), "Variable \"z\" is not accessed")
      ]

  testDiagnostics "pycodestyle" "test.py" [__i|def f(x: int, y: str) -> None:
                                                 z = 1.0
                                               f("asdf", 42)
                                              |] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics []


main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
