{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-} -- For PlainString, CodeString, etc.
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module TestLib.LSP (
  findLspConfig
  , getPathAndNixEnvironmentClosure

  , doNotebookSession
  , doSession'
  , doSession''

  , Helpers.getDiagnosticRanges
  , Helpers.getDiagnosticRanges'
  , assertDiagnosticRanges
  , assertDiagnosticRanges'
  , assertDiagnosticRangesFromSource'
  , testDiagnostics
  , testDiagnosticsLabel
  , testDiagnosticsLabelDesired
  , testDiagnostics''

  , itHasHoverSatisfying

  , itFormatsAs
  , itFormatsAs'
  , itReformats
  , formatsAs

  , Helpers.getHoverOrException
  , Helpers.allHoverText
  , Helpers.containsAll

  , LspContext
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader
import Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.List as L
import Data.String.Interpolate
import qualified Data.Text as T hiding (filter)
import Data.Text hiding (filter, show)
import GHC.Int
import GHC.Stack
import Language.LSP.Protocol.Types
import Language.LSP.Test
import Language.LSP.Test.Helpers (LanguageServerConfig(..), LspContext, LspSessionOptions(..), defaultLspSessionOptions, withLspSession)
import qualified Language.LSP.Test.Helpers as Helpers
import System.FilePath
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits (waitUntil)
import TestLib.Types
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.IO
import UnliftIO.Process


doNotebookSession :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Text -> Text -> (Helpers.LspSessionInfo -> Session (ExampleT ctx m) a) -> ExampleT ctx m a
doNotebookSession = doSession' "main.ipynb"

doSession' :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Text -> Text -> Text -> (Helpers.LspSessionInfo -> Session (ExampleT ctx m) a) -> ExampleT ctx m a
doSession' filename lsName codeToUse cb = doSession'' filename lsName codeToUse [] cb

doSession'' :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Text -> Text -> Text -> [(FilePath, B.ByteString)] -> (Helpers.LspSessionInfo -> Session (ExampleT ctx m) a) -> ExampleT ctx m a
doSession'' filename lsName codeToUse extraFiles cb = do
  lspConfig <- findLspConfig lsName
  (pathToUse, closure) <- getPathAndNixEnvironmentClosure
  let lspSessionOptions = (defaultLspSessionOptions lspConfig) {
        lspSessionOptionsInitialFileName = T.unpack filename
        , lspSessionOptionsInitialLanguageKind = LanguageKind_Python
        , lspSessionOptionsInitialCode = codeToUse
        , lspSessionOptionsExtraFiles = extraFiles
        , lspSessionOptionsReadOnlyBinds = closure
        , lspSessionOptionsPathEnvVar = pathToUse
        }
  withLspSession lspSessionOptions cb

testDiagnostics :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Text -> FilePath -> LanguageKind -> Text -> ([Diagnostic] -> ExampleT ctx m ()) -> SpecFree ctx m ()
testDiagnostics name filename languageKind code = testDiagnostics' name filename languageKind code []

testDiagnostics' :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Text -> FilePath -> LanguageKind -> Text -> [(FilePath, B.ByteString)] -> ([Diagnostic] -> ExampleT ctx m ()) -> SpecFree ctx m ()
testDiagnostics' name filename languageKind codeToTest = testDiagnostics'' [i|#{name}, #{filename} with #{show codeToTest} (diagnostics)|] name filename languageKind codeToTest

testDiagnosticsLabel :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => String -> Text -> FilePath -> LanguageKind -> Text -> ([Diagnostic] -> ExampleT ctx m ()) -> SpecFree ctx m ()
testDiagnosticsLabel label name filename languageKind codeToTest = testDiagnostics'' label name filename languageKind codeToTest []

testDiagnosticsLabelDesired :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => String -> Text -> FilePath -> LanguageKind -> Text -> ([Diagnostic] -> Bool) -> SpecFree ctx m ()
testDiagnosticsLabelDesired label name filename languageKind code cb = it label $ do
  lspConfig <- findLspConfig name
  (pathToUse, closure) <- getPathAndNixEnvironmentClosure

  let lspSessionOptions = (defaultLspSessionOptions lspConfig) {
        lspSessionOptionsInitialFileName = filename
        , lspSessionOptionsInitialLanguageKind = languageKind
        , lspSessionOptionsInitialCode = code
        , lspSessionOptionsReadOnlyBinds = closure
        , lspSessionOptionsPathEnvVar = pathToUse
        }

  Helpers.testDiagnostics lspSessionOptions languageKind $ \diags ->
    if | cb diags -> return ()
       | otherwise -> expectationFailure [i|Got unexpected diagnostics: #{diags}|]

testDiagnostics'' :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => String -> Text -> FilePath -> LanguageKind -> Text -> [(FilePath, B.ByteString)] -> ([Diagnostic] -> ExampleT ctx m ()) -> SpecFree ctx m ()
testDiagnostics'' label name filename languageKind code extraFiles cb = it label $ do
  lspConfig <- findLspConfig name
  (pathToUse, closure) <- getPathAndNixEnvironmentClosure

  let lspSessionOptions = (defaultLspSessionOptions lspConfig) {
        lspSessionOptionsInitialFileName = filename
        , lspSessionOptionsInitialLanguageKind = languageKind
        , lspSessionOptionsInitialCode = code
        , lspSessionOptionsReadOnlyBinds = closure
        , lspSessionOptionsPathEnvVar = pathToUse
        , lspSessionOptionsExtraFiles = extraFiles
        }

  Helpers.testDiagnostics lspSessionOptions languageKind $ \diags -> do
    lift $ cb diags

itHasHoverSatisfying :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Text -> FilePath -> LanguageKind -> Text -> Position -> (Hover -> ExampleT ctx m ()) -> SpecFree ctx m ()
itHasHoverSatisfying name filename languageKind code pos cb = it [i|#{name}: #{show code} (hover)|] $ do
  lspConfig <- findLspConfig name
  (pathToUse, closure) <- getPathAndNixEnvironmentClosure

  let lspSessionOptions = (defaultLspSessionOptions lspConfig) {
        lspSessionOptionsInitialFileName = filename
        , lspSessionOptionsInitialLanguageKind = languageKind
        , lspSessionOptionsInitialCode = code
        , lspSessionOptionsReadOnlyBinds = closure
        , lspSessionOptionsPathEnvVar = pathToUse
        }

  withLspSession lspSessionOptions $ \_ -> do
    ident <- openDoc filename languageKind
    getHover ident pos >>= \case
      Nothing -> expectationFailure [i|Expected a hover.|]
      Just x -> lift $ cb x

-- | Format a document and check the result, which is the only way to tell a working formatter
-- from a server that advertises documentFormattingProvider and then can't find its formatter.
itFormatsAs :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Text -> FilePath -> LanguageKind -> Text -> Text -> SpecFree ctx m ()
itFormatsAs = itFormatsAs' 180

-- | 'itFormatsAs' with an explicit timeout, for servers that need to load a project first.
itFormatsAs' :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Double -> Text -> FilePath -> LanguageKind -> Text -> Text -> SpecFree ctx m ()
itFormatsAs' timeoutSeconds name filename languageKind code expected =
  itFormats' timeoutSeconds name filename languageKind code (`shouldBe` expected)

-- | Weaker than 'itFormatsAs': only that formatting rewrote the document. For formatters whose
-- exact output we haven't pinned down, this still catches a server that advertises formatting
-- and can't deliver it.
itReformats :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Text -> FilePath -> LanguageKind -> Text -> SpecFree ctx m ()
itReformats name filename languageKind code =
  itFormats' 180 name filename languageKind code (`shouldNotBe` code)

itFormats' :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Double -> Text -> FilePath -> LanguageKind -> Text -> (Text -> ExampleT ctx m ()) -> SpecFree ctx m ()
-- Deliberately keeps the code out of the test name: Sandwich names the test's directory after
-- it, and the escaped newlines turn into backslashes in a path, which R.cache rewrites to
-- forward slashes and then can't write to.
itFormats' timeoutSeconds name filename languageKind code cb =
  it [i|#{name} formats #{filename}|] $
    formats' timeoutSeconds name filename languageKind code cb

-- | The body of 'itFormatsAs', for specs that need to wrap it (e.g. to mark it 'pending').
formatsAs :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Double -> Text -> FilePath -> LanguageKind -> Text -> Text -> ExampleT ctx m ()
formatsAs timeoutSeconds name filename languageKind code expected =
  formats' timeoutSeconds name filename languageKind code (`shouldBe` expected)

formats' :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Double -> Text -> FilePath -> LanguageKind -> Text -> (Text -> ExampleT ctx m ()) -> ExampleT ctx m ()
formats' timeoutSeconds name filename languageKind code cb = do
  lspConfig <- findLspConfig name
  (pathToUse, closure) <- getPathAndNixEnvironmentClosure

  let lspSessionOptions = (defaultLspSessionOptions lspConfig) {
        lspSessionOptionsInitialFileName = filename
        , lspSessionOptionsInitialLanguageKind = languageKind
        , lspSessionOptionsInitialCode = code
        , lspSessionOptionsReadOnlyBinds = closure
        , lspSessionOptionsPathEnvVar = pathToUse
        }

  withLspSession lspSessionOptions $ \_ -> do
    ident <- openDoc filename languageKind
    -- formatDoc applies the edits to the session's copy of the document, so retrying after
    -- the server has warmed up is harmless: a formatted document just comes back unchanged.
    waitUntil timeoutSeconds $ do
      formatDoc ident formattingOptions
      documentContents ident >>= lift . cb

formattingOptions :: FormattingOptions
formattingOptions = FormattingOptions 2 True Nothing Nothing Nothing

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
  MonadUnliftIO m, MonadLogger m
  , MonadReader context m, HasBaseContext context, Sandwich.HasLabel context "nixEnvironment" FilePath
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
assertDiagnosticRanges = assertDiagnosticRanges'' Helpers.getDiagnosticRanges

assertDiagnosticRanges' :: (HasCallStack, MonadIO m) => [Diagnostic] -> [(Range, Maybe (Int32 |? Text), Text)] -> m ()
assertDiagnosticRanges' = assertDiagnosticRanges'' Helpers.getDiagnosticRanges'

assertDiagnosticRanges'' :: (HasCallStack, MonadIO m, Eq a, ToJSON a) => ([Diagnostic] -> [a]) -> [Diagnostic] -> [a] -> m ()
assertDiagnosticRanges'' keyFn diagnostics desired = if
  | keyFn diagnostics == desired -> return ()
  | otherwise ->
      expectationFailure [__i|Got wrong diagnostics!

                              Expected: #{A.encode desired}

                              Found: #{A.encode $ keyFn diagnostics}
                             |]

assertDiagnosticRangesFromSource' :: (HasCallStack, MonadIO m) => Text -> [Diagnostic] -> [(Range, Maybe (Int32 |? Text), Text)] -> m ()
assertDiagnosticRangesFromSource' src diagnostics =
  assertDiagnosticRanges' (L.filter (\d -> _source d == Just src) diagnostics)
