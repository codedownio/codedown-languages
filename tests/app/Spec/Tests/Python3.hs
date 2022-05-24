
module Spec.Tests.Python3 (tests) where

import Control.Monad
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson as A
import Data.Aeson.QQ
import Data.Default
import Data.String.Interpolate
import qualified Data.Text as T hiding (filter)
import Data.Text hiding (filter)
import Language.LSP.Test
import System.FilePath
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.Types
import UnliftIO.Directory
import UnliftIO.Process


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
