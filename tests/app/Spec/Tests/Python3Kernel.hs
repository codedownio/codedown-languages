
module Spec.Tests.Python3Kernel (tests) where

import Control.Monad
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Unlift
import Data.String.Interpolate
import Data.Text
import qualified Data.Text as T
import Test.Sandwich
import TestLib.JupyterRunnerContext
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.Types
import UnliftIO.Process


kernelSpec = NixKernelSpec {
  nixKernelChannel = "codedown"
  , nixKernelLanguage = "python3"
  , nixKernelDisplayName = Just "Python 3"
  , nixKernelPackages = [(nameOnly"tensorflow")]
  , nixKernelLanguageServers = []
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettingsSchema = Nothing
  , nixKernelSettings = Nothing
  }

tests :: TopSpec
tests = introduceNixEnvironment [kernelSpec] [] "Python 3" $ introduceJupyterRunner $ do
  testKernelStdout "python3" [i|print("hi")|] "hi\n"
  testKernelStdout "python3" [i|print(42)|] "42\n"

  testKernelStdout "python3" [i|import tensorflow|] ""

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
