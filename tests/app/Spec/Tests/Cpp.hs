{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Spec.Tests.Cpp (tests) where

import Data.String.Interpolate
import Data.Text
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types

import qualified Spec.Tests.Cpp.LSP as LSP


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

  LSP.tests

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
