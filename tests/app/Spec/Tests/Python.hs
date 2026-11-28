{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Python (tests) where

import Control.Monad (when)
import Data.String.Interpolate
import Data.Text
import Language.LSP.Protocol.Types
import System.Info (os)
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types

import qualified Spec.Tests.Python.VariableInspector as VariableInspector


tests :: LanguageSpec
tests = describe "Python" $ parallel $ do
  tests' ("python3", "python3")
  -- tests' ("python3", "python310")
  tests' ("python3", "python312")
  tests' ("python3", "python313")
  tests' ("python3", "python314")

  -- PyPy's bootstrap interpreter doesn't build on Darwin, so there's no kernel to test there.
  when (os /= "darwin") pypyTests

tests' :: (Text, Text) -> LanguageSpec
tests' (kernelName, pythonPackage) = introduceNixEnvironment [kernelSpec kernelName pythonPackage] [] [i|Python (#{pythonPackage})|] $ introduceJupyterRunner $ do
  testKernelSearchersNonempty "python3"
  testHasExpectedFields "python3"

  testKernelStdout "python3" [i|print("hi")|] "hi\n"
  testKernelStdout "python3" [i|print(42)|] "42\n"
  testKernelStdout' "python3" [i|import scipy|] Nothing

  VariableInspector.tests "python3"

  testDiagnostics "python-lsp-server" "test.py" LanguageKind_Python [i|\n\n\nfoo = 42|] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics []

  testDiagnostics "pylint" "test.py" LanguageKind_Python [i|\n\n\nfoo = 42|] $ \diagnostics -> do
    assertDiagnosticRanges' diagnostics [
      (Range (Position 3 0) (Position 3 0), Nothing, "Final newline missing (C0304:missing-final-newline)")
      , (Range (Position 0 0) (Position 0 0), Nothing, "Missing module docstring (C0114:missing-module-docstring)")
      , (Range (Position 3 0) (Position 3 0), Nothing, "Disallowed name \"foo\" (C0104:disallowed-name)")
      ]

  testDiagnostics "pyright" "test.py" LanguageKind_Python [__i|\# pyright: strict
                                                               def f(x: int, y: str) -> None:
                                                                 z = 1.0
                                                               f("asdf", 42)
                                                              |] $ \diagnostics -> do
    assertDiagnosticRanges' diagnostics [
      (Range (Position 3 2) (Position 3 8), Just (InR "reportArgumentType"), "Argument of type \"Literal['asdf']\" cannot be assigned to parameter \"x\" of type \"int\" in function \"f\"\n\160\160\"Literal['asdf']\" is not assignable to \"int\"")
      , (Range (Position 3 10) (Position 3 12), Just (InR "reportArgumentType"), "Argument of type \"Literal[42]\" cannot be assigned to parameter \"y\" of type \"str\" in function \"f\"\n\160\160\"Literal[42]\" is not assignable to \"str\"")

      , (Range (Position 1 6) (Position 1 7), Nothing, "\"x\" is not accessed")
      , (Range (Position 1 14) (Position 1 15), Nothing, "\"y\" is not accessed")

      , (Range (Position 2 2) (Position 2 3), Nothing, "\"z\" is not accessed")
      , (Range (Position 2 2) (Position 2 3), Just (InR "reportUnusedVariable"), "Variable \"z\" is not accessed")
      ]

  testDiagnosticsServingConfig "pyright" "imports.py" LanguageKind_Python [__i|import scipy
                                                                               x: int = "a"
                                                                               x
                                                                              |] $ \diagnostics -> do
    assertDiagnosticRanges' diagnostics [
      (Range (Position 1 9) (Position 1 12), Just (InR "reportAssignmentType"), "Type \"Literal['a']\" is not assignable to declared type \"int\"\n\160\160\"Literal['a']\" is not assignable to \"int\"")
      , (Range (Position 0 7) (Position 0 12), Nothing, "\"scipy\" is not accessed")
      ]

  testDiagnosticsServingConfig "basedpyright" "test.py" LanguageKind_Python [__i|import scipy
                                                                                 def f(x: int, y: str) -> None:
                                                                                     pass
                                                                                 f("asdf", 42)
                                                                                 scipy
                                                                                |] $ \diagnostics -> do
    assertDiagnosticRanges' diagnostics [
      (Range (Position 1 6) (Position 1 7), Just (InR "reportUnusedParameter"), "\"x\" is not accessed")
      , (Range (Position 1 14) (Position 1 15), Just (InR "reportUnusedParameter"), "\"y\" is not accessed")

      , (Range (Position 3 2) (Position 3 8), Just (InR "reportArgumentType"), "Argument of type \"Literal['asdf']\" cannot be assigned to parameter \"x\" of type \"int\" in function \"f\"\n\160\160\"Literal['asdf']\" is not assignable to \"int\"")
      , (Range (Position 3 10) (Position 3 12), Just (InR "reportArgumentType"), "Argument of type \"Literal[42]\" cannot be assigned to parameter \"y\" of type \"str\" in function \"f\"\n\160\160\"Literal[42]\" is not assignable to \"str\"")
      ]

  testDiagnosticsServingConfigWithFiles "basedpyright" "test.py" LanguageKind_Python [__i|import mymod
                                                                                          def g(a):
                                                                                              return a
                                                                                          mymod
                                                                                         |] [("mymod.py", "foo = 1\n"), ("pyrightconfig.json", "{\"typeCheckingMode\": \"strict\"}")] $ \diagnostics -> do
    assertDiagnosticRanges' diagnostics [
      (Range (Position 1 4) (Position 1 5), Just (InR "reportUnknownParameterType"), "Return type is unknown")
      , (Range (Position 1 6) (Position 1 7), Just (InR "reportUnknownParameterType"), "Type of parameter \"a\" is unknown")
      , (Range (Position 1 6) (Position 1 7), Just (InR "reportMissingParameterType"), "Type annotation is missing for parameter \"a\"")
      , (Range (Position 2 11) (Position 2 12), Just (InR "reportUnknownVariableType"), "Return type is unknown")
      , (Range (Position 3 0) (Position 3 5), Just (InR "reportUnusedExpression"), "Expression value is unused")
      ]

  testDiagnosticsServingConfig "pyrefly" "test.py" LanguageKind_Python [__i|import scipy
                                                                            import notamodule
                                                                            scipy
                                                                            undefined_name
                                                                           |] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics [
      (Range (Position 1 7) (Position 1 17), Just (InR "missing-import"))
      , (Range (Position 3 0) (Position 3 14), Just (InR "unknown-name"))
      , (Range (Position 1 7) (Position 1 17), Just (InR "unused-import"))
      ]

  testDiagnosticsServingConfigWithFiles "pyrefly" "test.py" LanguageKind_Python [__i|import mymod
                                                                                     def f(x: int, y: str) -> None:
                                                                                         pass
                                                                                     f("asdf", 42)
                                                                                     mymod
                                                                                    |] [("mymod.py", "foo = 1\n"), ("pyrefly.toml", "preset = \"default\"\n")] $ \diagnostics -> do
    assertDiagnosticRanges' diagnostics [
      (Range (Position 3 2) (Position 3 8), Just (InR "bad-argument-type"), "Argument `Literal['asdf']` is not assignable to parameter `x` with type `int` in function `f`")
      , (Range (Position 3 10) (Position 3 12), Just (InR "bad-argument-type"), "Argument `Literal[42]` is not assignable to parameter `y` with type `str` in function `f`")
      ]

  testDiagnosticsServingConfig "ruff" "test.py" LanguageKind_Python [__i|import os
                                                                         x = 1
                                                                         import json
                                                                         x
                                                                        |] $ \diagnostics -> do
    assertDiagnosticRanges' diagnostics [
      (Range (Position 0 7) (Position 0 9), Just (InR "F401"), "`os` imported but unused\n\nhelp: Remove unused import: `os`")
      , (Range (Position 2 7) (Position 2 11), Just (InR "F401"), "`json` imported but unused\n\nhelp: Remove unused import: `json`")
      ]

  itFormatsAs "ruff" "test.py" LanguageKind_Python
    "def f( x,y ):\n  return x+y\nz   =   f(1,2)\n"
    "def f(x, y):\n    return x + y\n\n\nz = f(1, 2)\n"

  testDiagnostics "pycodestyle" "test.py" LanguageKind_Python [__i|def f(x: int, y: str) -> None:
                                                                     z = 1.0
                                                                   f("asdf", 42)
                                                                  |] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics []

  -- Jedi, the default language server, can't format at all; this is python-lsp-server with
  -- its default formatter. The run of blank lines survives because autopep8 reuses the
  -- pycodestyle `ignore` list, which has E303 in it (see pylsp_initialization_options.nix).
  itFormatsAs "python-lsp-server" "test.py" LanguageKind_Python
    "def f( x,y ):\n  return x+y\n\n\n\n\nz   =   f(1,2)\n"
    "def f(x, y):\n    return x + y\n\n\n\n\nz = f(1, 2)\n"


-- | Nixpkgs carries far less for PyPy than for CPython, so this covers the kernel itself
-- rather than the package and language server stack the CPython tests exercise.
pypyTests :: LanguageSpec
pypyTests = introduceNixEnvironment [pypyKernelSpec] [] "Python (pypy3)" $ introduceJupyterRunner $ do
  testKernelStdout "python3" [i|print("hi")|] "hi\n"
  testKernelStdout "python3" [i|import platform; print(platform.python_implementation())|] "PyPy\n"

pypyKernelSpec :: NixKernelSpec
pypyKernelSpec = NixKernelSpec {
  nixKernelName = "python3"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "PyPy"
  , nixKernelPackages = []
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelExtraConfig = Just [[i|python3Package = "pypy3"|]]
  }

kernelSpec :: Text -> Text -> NixKernelSpec
kernelSpec kernelName pythonPackage = NixKernelSpec {
  nixKernelName = kernelName
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "Python"
  , nixKernelPackages = [nameOnly "scipy"]
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelExtraConfig = Just [
      [i|python3Package = "#{pythonPackage}"|]

      , "lsp.jedi.enable = true"
      , "lsp.python-lsp-server.enable = true"
      , "lsp.pylint.enable = true"
      , "lsp.pyright.enable = true"
      , "lsp.basedpyright.enable = true"
      , "lsp.pyrefly.enable = true"
      , "lsp.ruff.enable = true"
      , [i|lsp.pyright.diagnosticSeverityOverrides.reportUnusedExpression = "none"|]
      , [i|lsp.basedpyright.diagnosticSeverityOverrides.reportUnusedExpression = "none"|]
      , "lsp.pycodestyle.enable = true"
      ]
  }


main :: IO ()
main = jupyterMain tests
