{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-} -- For PlainString, CodeString, etc.
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module TestLib.LSP where

import Control.Applicative
import Control.Lens hiding (List)
import Control.Monad
import Control.Monad.Catch as C (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Logger (MonadLogger, MonadLoggerIO)
import Control.Monad.Reader
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
import Data.Text hiding (filter, show)
import qualified Data.Text.IO as T
import GHC.Int
import GHC.Stack
import Language.LSP.Protocol.Capabilities
import Language.LSP.Protocol.Lens as LSP hiding (diagnostics, hover, id, label, name, ranges)
import Language.LSP.Protocol.Types
import Language.LSP.Test
import System.FilePath
import System.IO.Temp (createTempDirectory)
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits (waitUntil)
import TestLib.Aeson
import TestLib.Types
import UnliftIO.Async
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.IO
import UnliftIO.IORef
import UnliftIO.Process
import UnliftIO.STM


data LanguageServerType = LanguageServerTypeTcp
                        | LanguageServerTypeStream
  deriving (Show, Eq)
deriveJSON toSnakeC3 ''LanguageServerType

data LanguageServerConfig = LanguageServerConfig {
  lspConfigName :: Text
  , lspConfigVersion :: Maybe Text
  , lspConfigDescription :: Maybe Text
  , lspConfigDisplayName :: Maybe Text
  , lspConfigIcon :: Maybe FilePath
  , lspConfigExtensions :: [Text]
  , lspConfigAttrs :: S.Set Text
  , lspConfigType :: LanguageServerType
  , lspConfigPrimary :: Maybe Bool
  , lspConfigArgs :: [Text]
  , lspConfigLanguageId :: Maybe Text
  , lspConfigInitializationOptions :: Maybe A.Object
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

  , HasBaseContext ctx
  , HasMaybeBubblewrap ctx
  )

doNotebookSession :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Text -> Text -> (FilePath -> Session (ExampleT ctx m) a) -> ExampleT ctx m a
doNotebookSession = doSession' "main.ipynb"

doSession' :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Text -> Text -> Text -> (FilePath -> Session (ExampleT ctx m) a) -> ExampleT ctx m a
doSession' filename lsName codeToUse cb = doSession'' filename lsName codeToUse [] cb

doSession'' :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Text -> Text -> Text -> [(FilePath, B.ByteString)] -> (FilePath -> Session (ExampleT ctx m) a) -> ExampleT ctx m a
doSession'' filename lsName codeToUse extraFiles cb = do
  lspConfig <- findLspConfig lsName
  (pathToUse, closure) <- getPathAndNixEnvironmentClosure
  withLspSession lspConfig pathToUse closure (T.unpack filename) codeToUse extraFiles $ \_homeDir -> do
    cb (T.unpack filename)

testDiagnostics :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Text -> FilePath -> Maybe LanguageKind -> Text -> ([Diagnostic] -> ExampleT ctx m ()) -> SpecFree ctx m ()
testDiagnostics name filename maybeLanguageId codeToTest = testDiagnostics' name filename maybeLanguageId codeToTest []

testDiagnostics' :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Text -> FilePath -> Maybe LanguageKind -> Text -> [(FilePath, B.ByteString)] -> ([Diagnostic] -> ExampleT ctx m ()) -> SpecFree ctx m ()
testDiagnostics' name filename maybeLanguageId codeToTest = testDiagnostics'' [i|#{name}, #{filename} with #{show codeToTest} (diagnostics)|] name filename maybeLanguageId codeToTest

testDiagnosticsLabelDesired :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => String -> Text -> FilePath -> Maybe LanguageKind -> Text -> ([Diagnostic] -> Bool) -> SpecFree ctx m ()
testDiagnosticsLabelDesired label name filename maybeLanguageId codeToTest cb = it label $ do
  lspConfig <- findLspConfig name
  (pathToUse, closure) <- getPathAndNixEnvironmentClosure

  withLspSession' id lspConfig pathToUse closure filename codeToTest [] $ \_homeDir -> do
    _ <- openDoc filename (fromMaybe (LanguageKind_Custom name) maybeLanguageId)

    lastSeenDiagsVar <- newTVarIO mempty

    let watchDiagnostics = forever $ do
          diags <- waitForDiagnostics
          atomically $ writeTVar lastSeenDiagsVar diags

    withAsync watchDiagnostics $ \_ -> do
      waitUntil 60.0 $ do
        flip fix [] $ \loop lastValue ->
          if | cb lastValue -> return ()
             | otherwise -> do
                 newDiags <- atomically $ do
                   x <- readTVar lastSeenDiagsVar
                   when (x == lastValue) retrySTM
                   return x
                 loop newDiags

testDiagnosticsLabel :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => String -> Text -> FilePath -> Maybe LanguageKind -> Text -> ([Diagnostic] -> ExampleT ctx m ()) -> SpecFree ctx m ()
testDiagnosticsLabel label name filename maybeLanguageId codeToTest = testDiagnostics'' label name filename maybeLanguageId codeToTest []

testDiagnostics'' :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => String -> Text -> FilePath -> Maybe LanguageKind -> Text -> [(FilePath, B.ByteString)] -> ([Diagnostic] -> ExampleT ctx m ()) -> SpecFree ctx m ()
testDiagnostics'' label name filename maybeLanguageId codeToTest extraFiles cb = it label $ do
  lspConfig <- findLspConfig name
  (pathToUse, closure) <- getPathAndNixEnvironmentClosure

  withLspSession' id lspConfig pathToUse closure filename codeToTest extraFiles $ \_homeDir -> do
    _ <- openDoc filename (fromMaybe (LanguageKind_Custom name) maybeLanguageId)

    lastSeenDiagsVar <- newIORef mempty

    waitUntil 60.0 $ do
      diags <- waitForDiagnostics
      writeIORef lastSeenDiagsVar diags
      withException (lift $ cb diags) $ \(e :: SomeException) -> do
        lastSeenDiags <- readIORef lastSeenDiagsVar
        logError [i|Exception in testDiagnostics'': #{e}.\n\nLast seen diagnostics: #{A.encode lastSeenDiags}|]

itHasHoverSatisfying :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Text -> FilePath -> Maybe LanguageKind -> Text -> Position -> (Hover -> ExampleT ctx m ()) -> SpecFree ctx m ()
itHasHoverSatisfying name filename maybeLanguageId codeToTest pos cb = it [i|#{name}: #{show codeToTest} (hover)|] $ do
  lspConfig <- findLspConfig name
  (pathToUse, closure) <- getPathAndNixEnvironmentClosure

  withLspSession lspConfig pathToUse closure filename codeToTest [] $ \_homeDir -> do
    ident <- openDoc filename (fromMaybe (LanguageKind_Custom name) maybeLanguageId)
    getHover ident pos >>= \case
      Nothing -> expectationFailure [i|Expected a hover.|]
      Just x -> lift $ cb x

withLspSession :: (
  LspContext ctx m
  ) => LanguageServerConfig -> FilePath -> [FilePath] -> FilePath -> Text -> [(FilePath, B.ByteString)] -> (FilePath -> Session (ExampleT ctx m) a) -> ExampleT ctx m a
withLspSession = withLspSession' handleSessionException

handleSessionException :: MonadUnliftIO m => ExampleT ctx m a -> ExampleT ctx m a
handleSessionException = handle (\(e :: SessionException) -> expectationFailure [i|LSP session failed with SessionException: #{e}|])

withLspSession' :: (
  LspContext ctx m
  ) => (ExampleT ctx m a -> ExampleT ctx m a) -> LanguageServerConfig -> FilePath -> [FilePath] -> FilePath -> Text -> [(FilePath, B.ByteString)] -> (FilePath -> Session (ExampleT ctx m) a) -> ExampleT ctx m a
withLspSession' handleFn config pathToUse fullClosure filename codeToTest extraFiles session = do
  Just currentFolder <- getCurrentFolder

  homeDir <- liftIO $ createTempDirectory currentFolder "home"

  forM_ extraFiles $ \(path, bytes) -> do
    unless (isAbsolute path) $ do
      debug [i|Writing extra file: #{homeDir </> path}|]
      createDirectoryIfMissing True (homeDir </> takeDirectory path)
      liftIO $ B.writeFile (homeDir </> path) bytes

  createDirectoryIfMissing True (homeDir </> takeDirectory filename)

  -- Comment this and use openDoc' to simulate an unsaved document
  liftIO $ T.writeFile (homeDir </> filename) codeToTest

  let sessionConfig = def { lspConfig = fromMaybe mempty (lspConfigInitializationOptions config)
                          , logStdErr = True
                          , logMessages = True
                          , messageTimeout = 120
                          }

  let cmd:args = fmap T.unpack $ lspConfigArgs config
  (cp, modifyCp) <- getContext maybeBubblewrap >>= \case
    Nothing -> do
      let configEnv = maybe mempty (fmap (bimap T.unpack T.unpack) . M.toList) (lspConfigEnv config)
      let finalEnv = ("HOME", homeDir) : ("PATH", pathToUse) : configEnv
      info [i|Language server environment: #{finalEnv}|]
      let modifyCp cp = cp { env = Just finalEnv
                           , cwd = Just homeDir }
      return (proc cmd args, modifyCp)
    Just bwrapBinary -> do
      let bwrapArgs = ["--tmpfs", "/tmp"
                      , "--bind", homeDir, homeDir
                      , "--clearenv"
                      , "--setenv", "HOME", homeDir
                      , "--chdir", homeDir

                      , "--setenv", "PATH", pathToUse

                      , "--proc", "/proc"
                      , "--dev", "/dev"
                      ]
                      <> mconcat [["--ro-bind", x, x] | x <- fullClosure]
                      <> mconcat [["--setenv", T.unpack n, T.unpack v] | (n, v) <- M.toList (fromMaybe mempty (lspConfigEnv config))]
                      <> ["--"]
                      <> (cmd : args)

      return (proc bwrapBinary bwrapArgs, id)

  info [i|LSP command: #{cp}|]

  -- We don't support certain server-to-client requests, since the waitForDiagnostics doesn't handle them
  let caps = fullClientCapsForVersion (LSPVersion 3 16)
           & set (workspace . _Just . workspaceFolders) Nothing
           & set (workspace . _Just . configuration) Nothing
           & set (workspace . _Just . didChangeWatchedFiles . _Just . dynamicRegistration) (Just False)
           & set (workspace . _Just . didChangeConfiguration . _Just . dynamicRegistration) (Just False)
           & set (textDocument . _Just . semanticTokens . _Just . dynamicRegistration) (Just False)

  handleFn $ runSessionWithConfigCustomProcess modifyCp sessionConfig cp caps homeDir (session homeDir)

findLspConfig :: (
  MonadIO m, MonadLogger m, MonadReader context m, Sandwich.HasLabel context "nixEnvironment" FilePath
  ) => Text -> m LanguageServerConfig
findLspConfig name = do
  languageServersPath <- (</> "lib" </> "codedown" </> "language-servers") <$> getContext nixEnvironment
  languageServerFiles <- filter (\x -> ".yaml" `T.isSuffixOf` T.pack x) <$> listDirectory languageServersPath
  lspConfigs :: [LanguageServerConfig] <- (mconcat <$>) $ forM languageServerFiles $ \((languageServersPath </>) -> path) -> do
    liftIO (A.eitherDecodeFileStrict path) >>= \case
      Left err -> expectationFailure [i|Failed to decode language server path '#{path}': #{err}|]
      Right x -> return x

  config <- case L.find (\x -> lspConfigName x == name) lspConfigs of
    Nothing -> expectationFailure [i|Couldn't find LSP config: #{name}. Had: #{fmap lspConfigName lspConfigs}|]
    Just x -> do
      info [i|LSP config: #{A.encode x}|]
      return x

  return config

getBasicPath :: (
  MonadUnliftIO m, MonadLogger m, MonadReader context m, Sandwich.HasLabel context "nixEnvironment" FilePath
  ) => m FilePath
getBasicPath = do
  bracket (openFile "/dev/null" WriteMode) hClose $ \devNullHandle ->
    (T.unpack . T.strip . T.pack) <$> readCreateProcess ((proc "nix" ["run", ".#print-basic-path"]) { std_err = UseHandle devNullHandle }) ""

getPathAndNixEnvironmentClosure :: (
  MonadUnliftIO m, MonadLogger m, MonadReader context m, Sandwich.HasLabel context "nixEnvironment" FilePath
  ) => m (FilePath, [FilePath])
getPathAndNixEnvironmentClosure = do
  pathToUse <- getBasicPath

  -- Get the full closure of the Nix environment and jupyter runner
  nixEnv <- getContext nixEnvironment
  closure <- (fmap T.unpack . Prelude.filter (/= "") . T.splitOn "\n" . T.pack) <$> readCreateProcessWithLogging (
    proc "nix" (["path-info", "-r"
                , nixEnv
                ]
                <> (splitSearchPath pathToUse)
               )
    ) ""

  return (pathToUse, closure)

assertDiagnosticRanges :: (HasCallStack, MonadIO m) => [Diagnostic] -> [(Range, Maybe (Int32 |? Text))] -> ExampleT ctx m ()
assertDiagnosticRanges diagnostics desired = if
  | found == desired -> return ()
  | otherwise ->
      expectationFailure [__i|Got wrong diagnostics!

                              Expected: #{A.encode desired}

                              Found: #{A.encode found}
                             |]
  where
    found = getDiagnosticRanges diagnostics

getDiagnosticRanges :: [Diagnostic] -> [(Range, Maybe (Int32 |? Text))]
getDiagnosticRanges = fmap (\x -> (x ^. range, x ^. code))

assertDiagnosticRanges' :: (HasCallStack, MonadIO m) => [Diagnostic] -> [(Range, Maybe (Int32 |? Text), Text)] -> m ()
assertDiagnosticRanges' diagnostics desired = if
  | found == desired -> return ()
  | otherwise ->
      expectationFailure [__i|Got wrong diagnostics!

                              Expected: #{A.encode desired}

                              Found: #{A.encode found}
                             |]
  where
    found = getDiagnosticRanges' diagnostics

getDiagnosticRanges' :: [Diagnostic] -> [(Range, Maybe (Int32 |? Text), Text)]
getDiagnosticRanges' = fmap (\x -> (x ^. range, x ^. code, x ^. LSP.message))

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

type HoverContents = MarkupContent |? (MarkedString |? [MarkedString])

allHoverContentsText :: HoverContents -> Text
allHoverContentsText (InL (MarkupContent _ t)) = t
allHoverContentsText (InR markedStrings) = case markedStrings of
  InL ms -> markedStringToText ms
  InR mss -> mconcat $ fmap markedStringToText mss
  where
    markedStringToText (MarkedString (InL t)) = t
    markedStringToText (MarkedString (InR thing)) = thing ^. LSP.value

containsAll :: Text -> [Text] -> Bool
containsAll haystack = Prelude.all (`T.isInfixOf` haystack)
