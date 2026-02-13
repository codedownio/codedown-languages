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

printUnknownCode :: Text
printUnknownCode = [__i|package main
                        import ("fmt")
                        func main() {
                        fmt.Println(foo)
                        }|]

main :: IO ()
main = jupyterMain tests
