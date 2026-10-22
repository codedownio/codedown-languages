{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Java (tests) where

import Data.Aeson (Value(..))
import Data.String.Interpolate
import Data.Text (Text)
import Language.LSP.Protocol.Types
import Spec.Tests.Java.Common
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types

import qualified Spec.Tests.Java.Completion as Completion
import qualified Spec.Tests.Java.Diagnostics as Diagnostics
import qualified Spec.Tests.Java.Hovers as Hovers
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

    describe "LSP" $ do
      Diagnostics.tests

      Completion.tests

      Hovers.tests

      itFormatsAs lsName "test.java" LanguageKind_Java badlyFormattedCode formattedCode

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

badlyFormattedCode :: Text
badlyFormattedCode = "class Example {\nint   x =1+2;\nint doubled( ){\nreturn x*2;\n}\n}\n"

formattedCode :: Text
formattedCode = "class Example {\n  int x = 1 + 2;\n\n  int doubled() {\n    return x * 2;\n  }\n}\n"

main :: IO ()
main = jupyterMain tests
