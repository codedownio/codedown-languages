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
  , testDiagnostics
  , testDiagnosticsLabel
  , testDiagnosticsLabelDesired
  , testDiagnostics''

  , itHasHoverSatisfying

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
import Data.Maybe
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
import TestLib.Types
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.IO
import UnliftIO.Process


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
  let lspSessionOptions = (defaultLspSessionOptions lspConfig) {
        lspSessionOptionsInitialFileName = T.unpack filename
        , lspSessionOptionsInitialCode = codeToUse
        , lspSessionOptionsExtraFiles = extraFiles
        , lspSessionOptionsReadOnlyBinds = closure
        , lspSessionOptionsPathEnvVar = pathToUse
        }
  withLspSession lspSessionOptions $ \_homeDir -> do
    cb (T.unpack filename)

testDiagnostics :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Text -> FilePath -> Text -> ([Diagnostic] -> ExampleT ctx m ()) -> SpecFree ctx m ()
testDiagnostics name filename code = testDiagnostics' name filename code []

testDiagnostics' :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Text -> FilePath -> Text -> [(FilePath, B.ByteString)] -> ([Diagnostic] -> ExampleT ctx m ()) -> SpecFree ctx m ()
testDiagnostics' name filename codeToTest = testDiagnostics'' [i|#{name}, #{filename} with #{show codeToTest} (diagnostics)|] name filename codeToTest

testDiagnosticsLabel :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => String -> Text -> FilePath -> Text -> ([Diagnostic] -> ExampleT ctx m ()) -> SpecFree ctx m ()
testDiagnosticsLabel label name filename codeToTest = testDiagnostics'' label name filename codeToTest []

testDiagnosticsLabelDesired :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => String -> Text -> FilePath -> Text -> ([Diagnostic] -> Bool) -> SpecFree ctx m ()
testDiagnosticsLabelDesired label name filename code cb = it label $ do
  lspConfig <- findLspConfig name
  (pathToUse, closure) <- getPathAndNixEnvironmentClosure

  let lspSessionOptions = (defaultLspSessionOptions lspConfig) {
        lspSessionOptionsInitialFileName = filename
        , lspSessionOptionsInitialCode = code
        , lspSessionOptionsReadOnlyBinds = closure
        , lspSessionOptionsPathEnvVar = pathToUse
        }

  Helpers.testDiagnostics lspSessionOptions $ \diags ->
    if | cb diags -> return True
       | otherwise -> expectationFailure [i|Got unexpected diagnostics: #{diags}|]


testDiagnostics'' :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => String -> Text -> FilePath-> Text -> [(FilePath, B.ByteString)] -> ([Diagnostic] -> ExampleT ctx m ()) -> SpecFree ctx m ()
testDiagnostics'' label name filename code extraFiles cb = it label $ do
  lspConfig <- findLspConfig name
  (pathToUse, closure) <- getPathAndNixEnvironmentClosure

  let lspSessionOptions = (defaultLspSessionOptions lspConfig) {
        lspSessionOptionsInitialFileName = filename
        , lspSessionOptionsInitialCode = code
        , lspSessionOptionsReadOnlyBinds = closure
        , lspSessionOptionsPathEnvVar = pathToUse
        , lspSessionOptionsExtraFiles = extraFiles
        }

  Helpers.testDiagnostics lspSessionOptions $ \diags -> do
    lift $ cb diags
    return True


itHasHoverSatisfying :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Text -> FilePath -> Text -> Position -> (Hover -> ExampleT ctx m ()) -> SpecFree ctx m ()
itHasHoverSatisfying name filename code pos cb = it [i|#{name}: #{show code} (hover)|] $ do
  lspConfig <- findLspConfig name
  (pathToUse, closure) <- getPathAndNixEnvironmentClosure

  let lspSessionOptions = (defaultLspSessionOptions lspConfig) {
        lspSessionOptionsInitialFileName = filename
        , lspSessionOptionsInitialCode = code
        , lspSessionOptionsReadOnlyBinds = closure
        , lspSessionOptionsPathEnvVar = pathToUse
        }

  withLspSession lspSessionOptions $ \_ -> do
    ident <- openDoc filename (fromMaybe (LanguageKind_Custom "unknown") (lspConfigLanguageId lspConfig))
    getHover ident pos >>= \case
      Nothing -> expectationFailure [i|Expected a hover.|]
      Just x -> lift $ cb x

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
    found = Helpers.getDiagnosticRanges diagnostics

assertDiagnosticRanges' :: (HasCallStack, MonadIO m) => [Diagnostic] -> [(Range, Maybe (Int32 |? Text), Text)] -> m ()
assertDiagnosticRanges' diagnostics desired = if
  | found == desired -> return ()
  | otherwise ->
      expectationFailure [__i|Got wrong diagnostics!

                              Expected: #{A.encode desired}

                              Found: #{A.encode found}
                             |]
  where
    found = Helpers.getDiagnosticRanges' diagnostics
