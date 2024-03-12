{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Go (tests) where

import Data.String.Interpolate
import Language.LSP.Protocol.Types
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types


kernelSpec :: NixKernelSpec
kernelSpec = NixKernelSpec {
  nixKernelName = "go"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "Go"
  , nixKernelPackages = []
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Nothing
  }

tests :: LanguageSpec
tests = describe "Go" $ introduceNixEnvironment [kernelSpec] [] "Go" $ introduceJupyterRunner $ do
  testKernelSearchersBuild "go"
  testHasExpectedFields "go"

  testKernelStdout "go" [__i|import("fmt")
                             fmt.Println("hi")|] "hi\n"

  testDiagnostics "gopls" "test.go" Nothing [__i|package main
                                                 import ("fmt")
                                                 func main() {
                                                     fmt.Println(foo)
                                                 }|] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics [(Range (Position 3 16) (Position 3 19), Just (InR "UndeclaredName"))]

main :: IO ()
main = jupyterMain tests
