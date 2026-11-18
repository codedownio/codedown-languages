{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Clojure (tests) where

import Data.String.Interpolate
import Language.LSP.Protocol.Types
import Language.LSP.Test
import qualified Language.LSP.Test.Helpers as Helpers
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits (waitUntil)
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types

import qualified Spec.Tests.Clojure.VariableInspector as VariableInspector


kernelSpec :: NixKernelSpec
kernelSpec = NixKernelSpec {
  nixKernelName = "clojure"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "Clojure"
  , nixKernelPackages = []
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelExtraConfig = Nothing
  }

tests :: LanguageSpec
tests = describe "Clojure" $ introduceNixEnvironment [kernelSpec] [] "Clojure" $ introduceJupyterRunner $ do
  testKernelSearchersBuild "clojure"
  testHasExpectedFields "clojure"

  testKernelStdout "clojure" [__i|(println "hi")|] "hi\n"

  VariableInspector.tests "clojure"

  testDiagnostics "clojure-lsp" "test.clj" LanguageKind_Clojure [__i|(foo 42)|] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics [(Range (Position 0 1) (Position 0 4), Just (InR "unresolved-symbol"))]

  -- cljfmt reindents but deliberately leaves spacing within a line alone.
  itFormatsAs "clojure-lsp" "test.clj" LanguageKind_Clojure
    "(defn  foo [a   b]\n(+ a  b))\n"
    "(defn  foo [a   b]\n  (+ a  b))\n"

  it "finds a cell's function in workspace symbols" $ doNotebookSession "clojure-lsp" "(defn alpha-in-cell [] 2)\n" $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName LanguageKind_Clojure
    _ <- getDocumentSymbols ident

    waitUntil 60 $ do
      symbols <- symbolNames <$> getWorkspaceSymbols "alpha"
      symbols `listShouldContain` "alpha-in-cell"


main :: IO ()
main = jupyterMain tests
