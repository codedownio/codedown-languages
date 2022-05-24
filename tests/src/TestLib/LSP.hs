{-# LANGUAGE TemplateHaskell #-}

module TestLib.LSP where

import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson as A
import Data.Aeson.TH as A
import Data.Default
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Set as S
import Data.String.Interpolate
import qualified Data.Text as T hiding (filter)
import Data.Text hiding (filter)
import qualified Data.Text.IO as T
import Language.LSP.Test
import System.FilePath
import Test.Sandwich as Sandwich
import TestLib.Aeson
import TestLib.Types
import UnliftIO.Directory
import UnliftIO.Temporary


data LanguageServerType = LanguageServerTypeTcp
                        | LanguageServerTypeStream
  deriving (Show, Eq)
deriveJSON toSnakeC3 ''LanguageServerType

data LanguageServerConfig = LanguageServerConfig {
  lspConfigName :: Text
  , lspConfigDescription :: Maybe Text
  , lspConfigDisplayName :: Maybe Text
  , lspConfigIcon :: Maybe FilePath
  , lspConfigExtensions :: [Text]
  , lspConfigAttrs :: S.Set Text
  , lspConfigType :: LanguageServerType
  , lspConfigPrimary :: Maybe Bool
  , lspConfigArgs :: [Text]
  , lspConfigInitializationOptions :: Maybe A.Value
  , lspConfigConfigurationSettings :: Maybe A.Value
  , lspConfigNotebookSuffix :: Text
  , lspConfigKernelName :: Maybe Text
  , lspConfigEnv :: Maybe (Map Text Text)
  , lspConfigFile :: Maybe FilePath
  , lspConfigIsBuiltIn :: Maybe Bool
  } deriving (Show, Eq)
deriveJSON toSnake2 ''LanguageServerConfig

testDiagnostics :: (
  HasJupyterRunner context
  , HasNixEnvironment context
  , HasBaseContext context
  , MonadIO m
  , MonadBaseControl IO m
  , MonadUnliftIO m
  , MonadThrow m
  ) => Text -> FilePath -> Text -> [()] -> SpecFree context m ()
testDiagnostics name filename code desired = it [i|#{name}: #{show code}|] $ do
  Just currentFolder <- getCurrentFolder

  envPath <- (</> "lib" </> "codedown") <$> getContext nixEnvironment

  info [i|Saw envPath: #{envPath}|]
  languageServerFiles <- filter (\x -> "language-servers.yaml" `T.isSuffixOf` T.pack x) <$> listDirectory envPath
  lspConfigs :: [LanguageServerConfig] <- (mconcat <$>) $ forM languageServerFiles $ \((envPath </>) -> path) -> do
    liftIO (A.eitherDecodeFileStrict path) >>= \case
      Left err -> expectationFailure [i|Failed to decode language server path '#{path}': #{err}|]
      Right x -> return x

  config <- case L.find (\x -> lspConfigName x == name) lspConfigs of
    Nothing -> expectationFailure [i|Couldn't find LSP config: #{name}. Had: #{fmap lspConfigName lspConfigs}|]
    Just x -> return x

  let lspCommand = T.unpack $ T.unwords (lspConfigArgs config)
  info [i|LSP command: #{lspCommand}|]

  withTempDirectory currentFolder (T.unpack name) $ \dataDir -> do
    liftIO $ T.writeFile (dataDir </> filename) code

    withRunInIO $ \runInIO -> do
      runSessionWithConfig (def {lspConfig = lspConfigInitializationOptions config}) lspCommand fullCaps dataDir $ do
        openDoc filename "python3"
        diagnostics <- waitForDiagnostics
        liftIO $ runInIO $ info [i|Got diagnostics: #{A.encode diagnostics}|]
      -- diagnostics `shouldBe` desired
