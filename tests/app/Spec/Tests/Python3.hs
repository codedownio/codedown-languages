
module Spec.Tests.Python3 (tests) where

import Data.String.Interpolate
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes


kernelSpec = NixKernelSpec {
  nixKernelChannel = "codedown"
  , nixKernelLanguage = "python3"
  , nixKernelDisplayName = Just "Python 3"
  , nixKernelPackages = [nameOnly "tensorflow"]
  , nixKernelLanguageServers = [nameOnly "python-language-server"]
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettingsSchema = Nothing
  , nixKernelSettings = Nothing
  }

tests :: TopSpec
tests = describe "Python 3" $ introduceNixEnvironment [kernelSpec] [] "Python 3" $ introduceJupyterRunner $ do
  testKernelStdout "python3" [i|print("hi")|] "hi\n"
  testKernelStdout "python3" [i|print(42)|] "42\n"

  testKernelStdout "python3" [i|import tensorflow|] ""

  testDiagnostics "python-language-server" "test.py" [i|\n\n\nfoo = 42|] []



main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
