{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-} -- For PlainString, CodeString, etc.
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module TestLib.LSP where

import Control.Applicative
import Control.Lens hiding (List)
import Control.Monad
import Control.Monad.Catch as C (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson as A
import Data.Aeson.TH as A
import qualified Data.ByteString as B
import Data.Default
import Data.Function
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.String.Interpolate
import qualified Data.Text as T hiding (filter)
import Data.Text hiding (filter)
import qualified Data.Text.IO as T
import Data.Time
import GHC.Int
import Language.LSP.Test
import Language.LSP.Types
import Language.LSP.Types.Lens as LSP hiding (diagnostics, hover, label, name)
import System.FilePath
import System.IO.Temp (createTempDirectory)
import Test.Sandwich as Sandwich
import TestLib.Aeson
import TestLib.Types
import UnliftIO.Directory
import UnliftIO.Environment (getEnvironment)
import UnliftIO.Exception
import UnliftIO.MVar
import UnliftIO.Process


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
  , lspConfigLanguageId :: Maybe Text
  , lspConfigInitializationOptions :: Maybe A.Value
  , lspConfigNotebookSuffix :: Text
  , lspConfigKernelName :: Maybe Text
  , lspConfigEnv :: Maybe (Map Text Text)
  , lspConfigFile :: Maybe FilePath
  , lspConfigIsBuiltIn :: Maybe Bool
  } deriving (Show, Eq)
deriveJSON toSnake2 ''LanguageServerConfig

type LspContext ctx m = (
  Alternative m
  , MonadIO m
  , MonadBaseControl IO m
  , MonadUnliftIO m
  , MonadCatch m
  , MonadThrow m
  , MonadMask m

  , HasNixEnvironment ctx
  , HasBaseContext ctx
  )

doNotebookSession :: (
  LspContext ctx m
  ) => Text -> Text -> (FilePath -> Session (ExampleT ctx m) a) -> ExampleT ctx m a
doNotebookSession = doSession' "main.ipynb"

doSession' :: (
  LspContext ctx m
  ) => Text -> Text -> Text -> (FilePath -> Session (ExampleT ctx m) a) -> ExampleT ctx m a
doSession' filename lsName codeToUse cb = do
  withRunInIO $ \runInIO -> runInIO $ withLspSession lsName (T.unpack filename) codeToUse [] $ do
    cb (T.unpack filename)

testDiagnostics :: (
  LspContext ctx m
  ) => Text -> FilePath -> Maybe Text -> Text -> ([Diagnostic] -> ExampleT ctx m ()) -> SpecFree ctx m ()
testDiagnostics name filename maybeLanguageId codeToTest = testDiagnostics' name filename maybeLanguageId codeToTest []

testDiagnostics' :: (
  LspContext ctx m
  ) => Text -> FilePath -> Maybe Text -> Text -> [(FilePath, B.ByteString)] -> ([Diagnostic] -> ExampleT ctx m ()) -> SpecFree ctx m ()
testDiagnostics' name filename maybeLanguageId codeToTest = testDiagnostics'' [i|#{name}, #{filename} with #{show codeToTest} (diagnostics)|] name filename maybeLanguageId codeToTest

testDiagnostics'' :: (
  LspContext ctx m
  ) => String -> Text -> FilePath -> Maybe Text -> Text -> [(FilePath, B.ByteString)] -> ([Diagnostic] -> ExampleT ctx m ()) -> SpecFree ctx m ()
testDiagnostics'' label name filename maybeLanguageId codeToTest extraFiles cb = it label $ do
  lastFailureReason <- newMVar Nothing
  withRunInIO $ \runInIO ->
    runInIO $ withLspSession' (handle (handleFn lastFailureReason)) name filename codeToTest extraFiles $ do
      _ <- openDoc filename (fromMaybe name maybeLanguageId)
      -- _ <- openDoc' filename (fromMaybe name maybeLanguageId) codeToTest

      startTime <- liftIO getCurrentTime
      fix $ \loop -> do
        diagnostics <- waitForDiagnostics

        liftIO (try (runInIO $ cb diagnostics)) >>= \case
          Left (x :: FailureReason) -> do
            warn [i|testDiagnostics'' failure: #{x}|]
            _ <- liftIO $ modifyMVar_ lastFailureReason (const $ return $ Just x)
            now <- liftIO getCurrentTime
            if | (diffUTCTime now startTime) > (120 * 60 * 1_000_000) -> return ()
               | otherwise -> loop
          Right () -> return ()
  where
    handleFn :: (MonadCatch n, MonadIO n, MonadLogger n) => MVar (Maybe FailureReason) -> SessionException -> n a
    handleFn lastFailureReason e@(Timeout {}) = readMVar lastFailureReason >>= \case
      Nothing -> do
        warn [i|Had empty failure reason|]
        throwIO e
      Just x -> do
        warn [i|Had failure reason: #{x}|]
        throwIO x
    handleFn _ e = expectationFailure [i|LSP session failed with SessionException: #{e}|]

itHasHoverSatisfying :: (
  LspContext ctx m
  ) => Text -> FilePath -> Maybe Text -> Text -> Position -> (Hover -> ExampleT ctx m ()) -> SpecFree ctx m ()
itHasHoverSatisfying name filename maybeLanguageId codeToTest pos cb = it [i|#{name}: #{show codeToTest} (hover)|] $ do
  maybeHover <- withRunInIO $ \runInIO ->
    runInIO $ withLspSession name filename codeToTest [] $ do
      ident <- openDoc filename (fromMaybe name maybeLanguageId)
      getHover ident pos
  case maybeHover of
    Nothing -> expectationFailure [i|Expected a hover.|]
    Just x -> cb x

withLspSession :: (
  LspContext ctx m
  ) => Text -> FilePath -> Text -> [(FilePath, B.ByteString)] -> Session (ExampleT ctx m) a -> ExampleT ctx m a
withLspSession = withLspSession' (handle (\(e :: SessionException) -> expectationFailure [i|LSP session failed with SessionException: #{e}|]))

withLspSession' :: (
  LspContext ctx m
  ) => (ExampleT ctx m a -> ExampleT ctx m a) -> Text -> FilePath -> Text -> [(FilePath, B.ByteString)] -> Session (ExampleT ctx m) a -> ExampleT ctx m a
withLspSession' handleFn name filename codeToTest extraFiles session = do
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

  let cmd:args = fmap T.unpack $ lspConfigArgs config
  let lspCommand = proc cmd args
  info [i|LSP command: #{lspCommand}|]

  homeDir <- liftIO $ createTempDirectory currentFolder (T.unpack (name <> "_home"))
  dataDir <- liftIO $ createTempDirectory currentFolder (T.unpack name)

  forM_ extraFiles $ \(path, bytes) -> do
    unless (isAbsolute path) $ do
      case takeDirectory path of
        "." -> return ()
        initialDirs -> do
          createDirectoryIfMissing True (dataDir </> initialDirs)
      debug [i|Writing extra file: #{dataDir </> path}|]
      liftIO $ B.writeFile (dataDir </> path) bytes

  createDirectoryIfMissing True (dataDir </> takeDirectory filename)

  -- Comment this and use openDoc' above to simulate an unsaved document
  liftIO $ T.writeFile (dataDir </> filename) codeToTest

  let sessionConfig = def { lspConfig = lspConfigInitializationOptions config
                          , logStdErr = True
                          , logMessages = True
                          }

  env <- getEnvironment
  -- let cleanEnv = [(k, v) | (k, v) <- env, k /= "PATH", k /= "HOME", k /= "GHC_PACKAGE_PATH"]
  let cleanEnv = [(k, v) | (k, v) <- env, k /= "GHC_PACKAGE_PATH"]
  let configEnv = maybe mempty (fmap (bimap T.unpack T.unpack) . M.toList) (lspConfigEnv config)
  let finalEnv = ("HOME", homeDir) : (configEnv <> cleanEnv)
  info [i|Language server environment: #{finalEnv}|]
  let modifyCp cp = cp { env = Just finalEnv
                       , cwd = Just homeDir }

  -- We don't support certain server-to-client requests, since the waitForDiagnostics doesn't handle them
  let caps = fullCaps
           & set (workspace . _Just . workspaceFolders) Nothing
           & set (workspace . _Just . configuration) Nothing
           & set (workspace . _Just . didChangeWatchedFiles . _Just . dynamicRegistration) (Just False)
           & set (workspace . _Just . didChangeConfiguration . _Just . dynamicRegistration) (Just False)

  handleFn $ runSessionWithConfigCustomProcess modifyCp sessionConfig lspCommand caps dataDir session

assertDiagnosticRanges :: MonadThrow m => [Diagnostic] -> [(Range, Maybe (Int32 |? Text))] -> ExampleT ctx m ()
assertDiagnosticRanges diagnostics desired = ranges `shouldBe` desired
  where
    ranges = fmap (\x -> (x ^. range, x ^. code)) diagnostics

assertDiagnosticRanges' :: MonadThrow m => [Diagnostic] -> [(Range, Maybe (Int32 |? Text), Text)] -> ExampleT ctx m ()
assertDiagnosticRanges' diagnostics desired = ranges `shouldBe` desired
  where
    ranges = fmap (\x -> (x ^. range, x ^. code, x ^. LSP.message)) diagnostics

-- hoverShouldSatisfy :: MonadThrow m => Position -> (Maybe Hover -> ExampleT ctx m ()) -> ExampleT ctx m ()
-- hoverShouldSatisfy pos pred = getHover (TextDocumentIdentifier (Uri undefined)) pos >>= pred

getHoverOrException :: (
  MonadLoggerIO m, MonadThrow m, MonadUnliftIO m, Alternative m
  ) => TextDocumentIdentifier -> Position -> Session m Hover
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

containsAll :: Text -> [Text] -> Bool
containsAll haystack = Prelude.all (`T.isInfixOf` haystack)
