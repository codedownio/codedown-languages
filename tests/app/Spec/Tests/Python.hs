{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Python (tests) where

import Data.String.Interpolate
import Data.Text
import Language.LSP.Protocol.Types
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types


tests :: LanguageSpec
tests = describe "Python" $ parallel $ do
  tests' "python3"

  -- tests' "python2"
  -- tests' "pypy2"
  -- tests' "pypy3"


tests' :: Text -> LanguageSpec
tests' kernelName = describe [i|Python (#{kernelName})|] $ introduceNixEnvironment [kernelSpec kernelName] [] "Python 3" $ introduceJupyterRunner $ do
  testKernelSearchersNonempty kernelName
  testHasExpectedFields kernelName

  testKernelStdout kernelName [i|print("hi")|] "hi\n"
  testKernelStdout kernelName [i|print(42)|] "42\n"
  testKernelStdout' kernelName [i|import scipy|] Nothing

  testDiagnostics "python-lsp-server" "test.py" Nothing [i|\n\n\nfoo = 42|] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics []

  testDiagnostics "pylint" "test.py" Nothing [i|\n\n\nfoo = 42|] $ \diagnostics -> do
    assertDiagnosticRanges' diagnostics [
      (Range (Position 3 0) (Position 3 0), Nothing, "Final newline missing (C0304:missing-final-newline)")
      , (Range (Position 0 0) (Position 0 0), Nothing, "Missing module docstring (C0114:missing-module-docstring)")
      , (Range (Position 3 0) (Position 3 0), Nothing, "Disallowed name \"foo\" (C0104:disallowed-name)")
      ]

  testDiagnostics "pyright" "test.py" Nothing [__i|\# pyright: strict
                                                   def f(x: int, y: str) -> None:
                                                     z = 1.0
                                                   f("asdf", 42)
                                                  |] $ \diagnostics -> do
    assertDiagnosticRanges' diagnostics [
      (Range (Position 3 2) (Position 3 8), Just (InR "reportArgumentType"), "Argument of type \"Literal['asdf']\" cannot be assigned to parameter \"x\" of type \"int\" in function \"f\"\n\160\160\"Literal['asdf']\" is incompatible with \"int\"")
      , (Range (Position 3 10) (Position 3 12), Just (InR "reportArgumentType"), "Argument of type \"Literal[42]\" cannot be assigned to parameter \"y\" of type \"str\" in function \"f\"\n\160\160\"Literal[42]\" is incompatible with \"str\"")

      , (Range (Position 1 6) (Position 1 7), Nothing, "\"x\" is not accessed")
      , (Range (Position 1 14) (Position 1 15), Nothing, "\"y\" is not accessed")

      , (Range (Position 2 2) (Position 2 3), Nothing, "\"z\" is not accessed")
      , (Range (Position 2 2) (Position 2 3), Just (InR "reportUnusedVariable"), "Variable \"z\" is not accessed")
      ]

  testDiagnostics "pycodestyle" "test.py" Nothing [__i|def f(x: int, y: str) -> None:
                                                         z = 1.0
                                                       f("asdf", 42)
                                                      |] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics []


kernelSpec :: Text -> NixKernelSpec
kernelSpec kernelName = NixKernelSpec {
  nixKernelName = kernelName
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "Python"
  , nixKernelPackages = [nameOnly "scipy"]
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelExtraConfig = Just [
      "lsp.jedi.enable = true"
      , "lsp.python-lsp-server.enable = true"
      , "lsp.pylint.enable = true"
      , "lsp.pyright.enable = true"
      , "lsp.pycodestyle.enable = true"
      ]
  }


main :: IO ()
main = jupyterMain tests
