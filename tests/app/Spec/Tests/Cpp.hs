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

import qualified Spec.Tests.Cpp.Completion as Completion
import qualified Spec.Tests.Cpp.Hovers as Hovers


tests :: LanguageSpec
tests = do
  describe "C++" $ do
    testKernelSearchersBuild "cpp"
    testHasExpectedFields "cpp"

  parallel $ do
    tests' "c++17"
    tests' "c++20"
    tests' "c++23"
    tests' "c++2c"

tests' :: Text -> LanguageSpec
tests' flavor = describe [i|C++ (#{flavor})|] $ introduceNixEnvironment [kernelSpecWithLsp flavor] [] "C++ Nix env" $ introduceJupyterRunner $ do
  describe "Kernel tests" $ do
    testKernelStdout "cpp" [__i|\#include <iostream>
                                using namespace std;
                                cout << "hi" << endl;|] "hi\n"

  describe "LSP" $ do
    testDiagnostics'' "simple" lsName "test.cpp" LanguageKind_CPP
      [__i|int main() {
             undefined_function();
             return 0;
           }|] [] $ \diags -> do
          info [i|Got diags: #{diags}|]
          info [i|Got ranges: #{getDiagnosticRanges' diags}|]
          getDiagnosticRanges' diags `shouldBe` [(Range (Position 1 2) (Position 1 20), Just (InR "undeclared_var_use"), "Use of undeclared identifier 'undefined_function'")]

    Completion.tests

    Hovers.tests

lsName :: Text
lsName = "clangd"

kernelSpecWithLsp :: Text -> NixKernelSpec
kernelSpecWithLsp flavor = kernelSpec' [
  [i|flavor = "#{flavor}"|]
  , "lsp.clangd.enable = true"
  , "lsp.clangd.debug = true"
  -- , "lsp.clangd.super-debug = true"
  ]

kernelSpec' :: [Text] -> NixKernelSpec
kernelSpec' extraConfig = NixKernelSpec {
  nixKernelName = "cpp"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "C++"
  , nixKernelPackages = []
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelExtraConfig = Just extraConfig
  }

main :: IO ()
main = jupyterMain tests
