{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Clojure (tests) where

import Data.String.Interpolate
import Language.LSP.Protocol.Types
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers


kernelSpec :: NixKernelSpec
kernelSpec = NixKernelSpec {
  nixKernelName = "clojure"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "Clojure"
  , nixKernelPackages = []
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Nothing
  }

tests :: TopSpec
tests = describe "Clojure" $ introduceNixEnvironment [kernelSpec] [] "Clojure" $ introduceJupyterRunner $ do
  testKernelSearchersBuild "clojure"

  testKernelStdout "clojure" [__i|(println "hi")|] "hi\n"

  testDiagnostics "clojure-lsp" "test.clj" Nothing [__i|(foo 42)|] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics [(Range (Position 0 1) (Position 0 4), Just (InR "unresolved-symbol"))]


main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
