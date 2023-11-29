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
  -- tests' "cpp98"
  tests' "cpp11"
  tests' "cpp14"
  tests' "cpp17"
  tests' "cpp20"
  tests' "cpp23"

tests' :: Text -> LanguageSpec
tests' kernelName = describe [i|C++ (#{kernelName})|] $ introduceNixEnvironment [kernelSpec kernelName] [] "C++" $ introduceJupyterRunner $ do
  testKernelSearchersBuild kernelName

  testKernelStdout kernelName [__i|\#include <iostream>
                                   using namespace std;
                                   cout << "hi" << endl;|] "hi\n"


kernelSpec :: Text -> NixKernelSpec
kernelSpec kernelName  = NixKernelSpec {
  nixKernelName = kernelName
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "CPP"
  , nixKernelPackages = []
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Nothing
  }

main :: IO ()
main = jupyterMain tests
