{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Spec.Tests.Cpp (tests) where

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
tests = describe "C++" $ parallel $ do
  testKernelSearchersBuild "cpp"
  testHasExpectedFields "cpp"

  -- tests' "cpp98"
  -- tests' "c++11"
  -- tests' "c++14"
  tests' "c++17"
  tests' "c++20"
  tests' "c++23"
  tests' "c++2c"

  testsWithLsp "c++23"

tests' :: Text -> LanguageSpec
tests' flavor = describe [i|C++ (#{flavor})|] $ introduceNixEnvironment [kernelSpec flavor] [] "C++" $ introduceJupyterRunner $ do
  testKernelStdout "cpp" [__i|\#include <iostream>
                              using namespace std;
                              cout << "hi" << endl;|] "hi\n"

testsWithLsp :: Text -> LanguageSpec
testsWithLsp flavor = describe [i|C++ (#{flavor}) with LSP|] $ introduceNixEnvironment [kernelSpecWithLsp flavor] [] "C++" $ do
  describe "LSP" $ do
    testDiagnosticsLabelDesired "simple" lsName "test.cpp" (Just "cpp")
      [__i|int main() {
             undefined_function();
             return 0;
           }|]
      ((== [(Range (Position 1 2) (Position 1 20), Nothing, "use of undeclared identifier 'undefined_function'")]) . getDiagnosticRanges')

lsName :: Text
lsName = "clangd"

kernelSpec :: Text -> NixKernelSpec
kernelSpec flavor  = NixKernelSpec {
  nixKernelName = "cpp"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "CPP"
  , nixKernelPackages = []
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelExtraConfig = Just [
      [i|flavor = "#{flavor}"|]
      ]
  }

kernelSpecWithLsp :: Text -> NixKernelSpec
kernelSpecWithLsp flavor = NixKernelSpec {
  nixKernelName = "cpp"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "CPP"
  , nixKernelPackages = []
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelExtraConfig = Just [
      [i|flavor = "#{flavor}"|]
      , "lsp.clangd.enable = true;"
      ]
  }

main :: IO ()
main = jupyterMain tests
