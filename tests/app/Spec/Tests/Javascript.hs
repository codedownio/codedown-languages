{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Javascript (tests) where

import Data.String.Interpolate
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types

import qualified Spec.Tests.Javascript.Completion as Completion
import qualified Spec.Tests.Javascript.Display as Display
import qualified Spec.Tests.Javascript.Hovers as Hovers
import qualified Spec.Tests.Javascript.Packages as Packages


tests :: LanguageSpec
tests = describe "JavaScript" $ do
  testKernelSearchersNonempty "javascript"
  testHasExpectedFields "javascript"

  introduceNixEnvironment [kernelSpec] [] "JavaScript" $ introduceJupyterRunner $ do
    describe "Kernel" $ do
      testKernelStdout "javascript" [__i|console.log("hi")|] "hi\n"

      -- tslab prints the value of a bare last expression rather than sending execute_result.
      testKernelStdout "javascript" [__i|var x = 41; x + 1|] "42\n"

      -- Cell state carries over, and top-level await works.
      testKernelStdout "javascript" [__i|const later = await Promise.resolve("done"); console.log(later)|] "done\n"

    Packages.tests

    Display.tests

    describe "LSP" $ do
      Completion.tests

      Hovers.tests

kernelSpec :: NixKernelSpec
kernelSpec = NixKernelSpec {
  nixKernelName = "javascript"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "JavaScript"
  , nixKernelPackages = map nameOnly ["d3", "jsdom", "@types/d3", "simple-statistics"]
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelExtraConfig = Just [
      "lsp.typescript-language-server.enable = true"
      ]
  }

main :: IO ()
main = jupyterMain tests
