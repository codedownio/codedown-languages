
module Spec.Tests.Bash (tests) where

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
  , nixKernelLanguage = "bashInteractive"
  , nixKernelDisplayName = Just "Bash"
  , nixKernelPackages = []
  , nixKernelLanguageServers = []
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettingsSchema = Nothing
  , nixKernelSettings = Nothing
  }

tests :: TopSpec
tests = describe "Bash" $ introduceNixEnvironment [kernelSpec] [] "Bash" $ introduceJupyterRunner $ do
  testKernelStdout "bash" [i|echo hi|] "hi\n"



main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
