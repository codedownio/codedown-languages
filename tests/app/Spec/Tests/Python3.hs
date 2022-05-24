
module Spec.Tests.Python3 (tests) where

import Control.Monad
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson as A
import Data.Aeson.QQ
import Data.Default
import Data.String.Interpolate
import Data.Text
import qualified Data.Text as T
import Language.LSP.Test
import System.FilePath
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.Types
import UnliftIO.Process


kernelSpec = NixKernelSpec {
  nixKernelChannel = "codedown"
  , nixKernelLanguage = "python3"
  , nixKernelDisplayName = Just "Python 3"
  , nixKernelPackages = [nameOnly "tensorflow"]
  , nixKernelLanguageServers = [nameOnly "python-lsp-server"]
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

  testDiagnostics "python3" [i|import tensorflow|] []

config :: Value
config = [aesonQQ|{pylsp: {plugins: {pycodestyle: {ignore: ["E303", "E402"]}}}}|]

testDiagnostics :: (
  HasJupyterRunner context
  , HasNixEnvironment context
  , HasBaseContext context
  , MonadIO m
  , MonadBaseControl IO m
  , MonadUnliftIO m
  , MonadThrow m
  ) => Text -> Text -> [()] -> SpecFree context m ()
testDiagnostics kernel code desired = it [i|#{kernel}: #{code} -> #{desired}|] $ do
  Just dataDir <- getCurrentFolder

  envPath <- (</> "lib" </> "codedown") <$> getContext nixEnvironment

  lspCommand <- undefined

  withRunInIO $ \runInIO -> do
    runSessionWithConfig (def { lspConfig = Just config }) lspCommand fullCaps dataDir $ do
      openDoc "test.py" "python3"
      diagnostics <- waitForDiagnostics
      liftIO $ runInIO $ info [i|Got diagnostics: #{diagnostics}|]
    -- diagnostics `shouldBe` desired


main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
