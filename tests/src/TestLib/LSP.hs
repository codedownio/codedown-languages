{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-} -- For PlainString, CodeString, etc.

module TestLib.LSP where

import Control.Lens hiding (List)
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
  withRunInIO $ \runInIO ->
    runInIO $ withLspSession name filename code $ do
      openDoc filename name
      diagnostics <- waitForDiagnostics
      liftIO $ runInIO $ info [i|Got diagnostics: #{A.encode diagnostics}|]
      liftIO $ runInIO $ cb diagnostics

itHasHoverSatisfying :: (
  HasNixEnvironment context
  , HasBaseContext context
  , MonadIO m
  , MonadBaseControl IO m
  , MonadUnliftIO m
  , MonadThrow m
  ) => Text -> FilePath -> Text -> Position -> (Hover -> ExampleT context m ()) -> SpecFree context m ()
itHasHoverSatisfying name filename code pos cb = it [i|#{name}: #{show code}|] $ do
  maybeHover <- withRunInIO $ \runInIO ->
    runInIO $ withLspSession name filename code $ do
      ident <- openDoc filename "haskell"
      getHover ident pos
  case maybeHover of
    Nothing -> expectationFailure [i|Expected a hover.|]
    Just x -> cb x

withLspSession :: (
  HasNixEnvironment context
  , HasBaseContext context
  , MonadIO m
  , MonadBaseControl IO m
  , MonadUnliftIO m
  , MonadThrow m
  ) => Text -> FilePath -> Text -> Session a -> ExampleT context m a
withLspSession name filename code doSession = do
  Just currentFolder <- getCurrentFolder

  languageServersPath <- (</> "lib" </> "codedown" </> "language-servers") <$> getContext nixEnvironment
  languageServerFiles <- filter (\x -> ".yaml" `T.isSuffixOf` T.pack x) <$> listDirectory languageServersPath
  lspConfigs :: [LanguageServerConfig] <- (mconcat <$>) $ forM languageServerFiles $ \((languageServersPath </>) -> path) -> do
    liftIO (A.eitherDecodeFileStrict path) >>= \case
      Left err -> expectationFailure [i|Failed to decode language server path '#{path}': #{err}|]
      Right x -> return x

  config <- case L.find (\x -> lspConfigName x == name) lspConfigs of
    Nothing -> expectationFailure [i|Couldn't find LSP config: #{name}. Had: #{fmap lspConfigName lspConfigs}|]
    Just x -> return x
  info [i|LSP config: #{A.encode config}|]

  let lspCommand = T.unpack $ T.unwords (lspConfigArgs config)
  info [i|LSP command: #{lspCommand}|]

  withTempDirectory currentFolder (T.unpack (name <> "_home")) $ \homeDir -> do
    withTempDirectory currentFolder (T.unpack name) $ \dataDir -> do
      liftIO $ T.writeFile (dataDir </> filename) code

      let sessionConfig = def { lspConfig = lspConfigInitializationOptions config
                              -- , logStdErr = True
                              -- , logMessages = True
                              }

      env <- getEnvironment
      let cleanEnv = [(k, v) | (k, v) <- env, k /= "PATH", k /= "HOME", k /= "GHC_PACKAGE_PATH"]
      let finalEnv = ("HOME", homeDir) : cleanEnv
      info [i|finalEnv: #{finalEnv}|]
      let modifyCp cp = cp { env = Just [] } -- Just finalEnv

      -- We don't support certain server-to-client requests, since the waitForDiagnostics doesn't handle them
      let caps = fullCaps
               & set (workspace . _Just . workspaceFolders) Nothing
               & set (workspace . _Just . configuration) Nothing

      liftIO $ runSessionWithConfigCustomProcess modifyCp sessionConfig lspCommand caps dataDir $ do
        doSession


assertDiagnosticRanges :: MonadThrow m => [Diagnostic] -> [(Range, Maybe (Int32 |? Text))] -> ExampleT context m ()
assertDiagnosticRanges diagnostics desired = ranges `shouldBe` desired
  where
    ranges = fmap (\x -> (x ^. range, x ^. code)) diagnostics

-- hoverShouldSatisfy :: MonadThrow m => Position -> (Maybe Hover -> ExampleT context m ()) -> ExampleT context m ()
-- hoverShouldSatisfy pos pred = getHover (TextDocumentIdentifier (Uri undefined)) pos >>= pred

getHoverOrException :: TextDocumentIdentifier -> Position -> Session Hover
getHoverOrException tdi pos = getHover tdi pos >>= \case
  Nothing -> expectationFailure [i|No hover returned.|]
  Just x -> return x

allHoverText :: Hover -> Text
allHoverText hover = allHoverContentsText (hover ^. contents)

allHoverContentsText :: HoverContents -> Text
allHoverContentsText (HoverContentsMS (List mss)) = mconcat $ fmap markedStringToText mss
  where
    markedStringToText (PlainString t) = t
    markedStringToText (CodeString (LanguageString _ t)) = t
allHoverContentsText (HoverContents (MarkupContent _ t)) = t
