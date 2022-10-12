
module Spec.Tests.Cpp (tests) where

import Data.String.Interpolate
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.NixEnvironmentContext
import TestLib.NixTypes


kernelSpec = NixKernelSpec {
  nixKernelName = "cpp11"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "CPP"
  , nixKernelPackages = []
  , nixKernelLanguageServers = []
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Nothing
  }

tests :: TopSpec
tests = describe "C++" $ introduceNixEnvironment [kernelSpec] [] "C++" $ introduceJupyterRunner $ do
  testKernelStdout "cpp11" [__i|\#include <iostream>
                                using namespace std;
                                cout << "hi" << endl;|] "hi\n"


main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
