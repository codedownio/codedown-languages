{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NumericUnderscores #-}

module TestLib.LSP where

import Control.Lens
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
import GHC.Int
import Language.LSP.Test
import Language.LSP.Types
import qualified Language.LSP.Types.Capabilities as C
import Language.LSP.Types.Lens
import System.FilePath
import System.IO
import Test.Sandwich as Sandwich
import TestLib.Aeson
import TestLib.Types
import UnliftIO.Async
import UnliftIO.Directory
import UnliftIO.Environment (getEnvironment)
import UnliftIO.Process
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
  HasNixEnvironment context
  , HasBaseContext context
  , MonadIO m
  , MonadBaseControl IO m
  , MonadUnliftIO m
  , MonadThrow m
  ) => Text -> FilePath -> Text -> ([Diagnostic] -> ExampleT context m ()) -> SpecFree context m ()
testDiagnostics name filename code cb = it [i|#{name}: #{show code}|] $ do
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
  info [i|LSP config: #{A.encode config}|]

  let lspCommand = T.unpack $ T.unwords (lspConfigArgs config)
  info [i|LSP command: #{lspCommand}|]

  diagnostics <-
    withTempDirectory currentFolder (T.unpack (name <> "_home")) $ \homeDir -> do
      withTempDirectory currentFolder (T.unpack name) $ \dataDir -> do
        liftIO $ T.writeFile (dataDir </> filename) code

        let sessionConfig = def { lspConfig = lspConfigInitializationOptions config
                                -- , logStdErr = True
                                -- , logMessages = True
                                }

        withRunInIO $ \runInIO -> do
          env <- getEnvironment
          let cleanEnv = [(k, v) | (k, v) <- env, k /= "PATH", k /= "HOME", k /= "GHC_PACKAGE_PATH"]
          let finalEnv = ("HOME", homeDir) : cleanEnv
          runInIO $ info [i|finalEnv: #{finalEnv}|]
          -- let modifyCp cp = cp { env = Just finalEnv }
          let modifyCp cp = cp { env = Just [] }

          runSessionWithConfig' modifyCp sessionConfig lspCommand fullCaps dataDir $ do
            openDoc filename name
            diagnostics <- waitForDiagnostics
            liftIO $ runInIO $ info [i|Got diagnostics: #{A.encode diagnostics}|]
            return diagnostics

  cb diagnostics



assertDiagnosticRanges :: MonadThrow m => [Diagnostic] -> [(Range, Maybe (Int32 |? Text))] -> ExampleT context m ()
assertDiagnosticRanges diagnostics desired = ranges `shouldBe` desired
  where
    ranges = fmap (\x -> (x ^. range, x ^. code)) diagnostics
