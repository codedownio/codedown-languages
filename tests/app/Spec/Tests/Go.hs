{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Go (tests) where

import Data.String.Interpolate
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types

import qualified Spec.Tests.Go.LSP as LSP


tests :: LanguageSpec
tests = describe "Go" $ do
  testKernelSearchersBuild "go"
  testHasExpectedFields "go"

  introduceNixEnvironment [kernelSpecWithLsp] [] "Go" $ introduceJupyterRunner $ do
    describe "Kernel tests" $ do
      testKernelStdout "go" [__i|import("fmt")
                                 fmt.Println("hi")|] "hi\n"

    LSP.tests

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

main :: IO ()
main = jupyterMain tests
