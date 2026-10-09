-- | Starting a language server out of a built Nix environment.
--
-- Split out of "TestLib.LSP" so that "TestLib.LSP.Formatting" can use it without the two
-- importing each other.

module TestLib.LSP.Session (
  findLspConfig
  , getPathAndNixEnvironmentClosure
  , lspSessionOptionsFor
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader
import Data.Aeson as A
import qualified Data.List as L
import Data.String.Interpolate
import qualified Data.Text as T
import Data.Text (Text)
import Language.LSP.Protocol.Types
import Language.LSP.Test.Helpers (LanguageServerConfig(..), LspContext, LspSessionOptions(..), defaultLspSessionOptions)
import System.FilePath
import Test.Sandwich as Sandwich
import TestLib.Types
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.IO
import UnliftIO.Process


-- | Options for a session over a single file, which is what nearly every LSP test wants.
lspSessionOptionsFor :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Text -> FilePath -> LanguageKind -> Text -> ExampleT ctx m LspSessionOptions
lspSessionOptionsFor name filename languageKind code = do
  lspConfig <- findLspConfig name
  (pathToUse, closure) <- getPathAndNixEnvironmentClosure

  return $ (defaultLspSessionOptions lspConfig) {
    lspSessionOptionsInitialFileName = filename
    , lspSessionOptionsInitialLanguageKind = languageKind
    , lspSessionOptionsInitialCode = code
    , lspSessionOptionsReadOnlyBinds = closure
    , lspSessionOptionsPathEnvVar = pathToUse
    }

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
