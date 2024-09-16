{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Cpp (tests) where

import Data.String.Interpolate
import Data.Text
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types


tests :: LanguageSpec
tests = describe "C++" $ parallel $ do
  testKernelSearchersBuild "cpp"
  testHasExpectedFields "cpp"

  -- tests' "cpp98"
  tests' "c++11"
  tests' "c++14"
  tests' "c++17"
  tests' "c++20"
  tests' "c++23"

tests' :: Text -> LanguageSpec
tests' flavor = describe [i|C++ (#{flavor})|] $ introduceNixEnvironment [kernelSpec flavor] [] "C++" $ introduceJupyterRunner $ do
  testKernelStdout "cpp" [__i|\#include <iostream>
                              using namespace std;
                              cout << "hi" << endl;|] "hi\n"


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

main :: IO ()
main = jupyterMain tests
