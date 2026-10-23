{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Java (tests) where

import Data.Aeson (Value(..))
import Data.String.Interpolate
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types

import qualified Spec.Tests.Java.LSP as LSP
import qualified Spec.Tests.Java.Packages as Packages


tests :: LanguageSpec
tests = describe "Java" $ do
  testKernelSearchersNonempty "java"
  testHasExpectedFields "java"

  introduceNixEnvironment [kernelSpec] [] "Java" $ introduceJupyterRunner $ do
    describe "Kernel" $ do
      testKernelStdout "java" [i|System.out.println("hi");|] "hi\n"

      -- JShell reports a trailing expression's value, so it arrives as an execute_result.
      itHasExecuteTexts "java" [i|41 + 1|] [Just (Array ["42"])]

      -- Snippets persist across execute requests.
      itHasExecuteTexts "java" [__i|int x = 41;
                                    @@@CELL@@@
                                    x + 1|] [Just (Array ["42"])]

      -- Has to be an error reply: "ok" plus a complaint on stdout looks like success.
      testKernelErrorContaining "java" [i|int y = "not an int";|] "incompatible types"

    Packages.tests

    LSP.tests

kernelSpec :: NixKernelSpec
kernelSpec = NixKernelSpec {
  nixKernelName = "java"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "Java"
  , nixKernelPackages = map nameOnly ["org.apache.commons:commons-lang3", "com.google.guava:guava"]
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelExtraConfig = Just [
      "lsp.jdtls.enable = true"
      ]
  }

main :: IO ()
main = jupyterMain tests
