{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module TestLib.TestBuilding where

import Conduit as C
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as A
import Data.ByteString.Lazy.Char8 as BL8
import Data.Function
import qualified Data.List as L
import Data.String.Interpolate
import Data.Text as T
import GHC.Stack
import System.Exit
import System.FilePath
import Test.Sandwich
import TestLib.JupyterRunnerContext
import TestLib.Types
import TestLib.Util
import UnliftIO.Directory
import UnliftIO.Environment
import UnliftIO.Process


testBuild :: (
  HasCallStack, MonadUnliftIO m, MonadLogger m, MonadReader context m
  , HasBaseContext context, HasBootstrapNixpkgs context
  ) => String -> m FilePath
testBuild = testBuild' LevelDebug

testBuild' :: (
  HasCallStack, MonadUnliftIO m, MonadLogger m, MonadReader context m
  , HasBaseContext context, HasBootstrapNixpkgs context
  ) => LogLevel -> String -> m FilePath
testBuild' logLevel expr = do
  rootDir <- findFirstParentMatching (\x -> doesPathExist (x </> ".git"))
  env <- getEnvWithNixPath
  let cp = (proc "nix-build" [".", "-A", expr, "--no-out-link"]) {
        cwd = Just rootDir
        , env = Just env
        }
  (T.unpack . T.strip . T.pack) <$> (readCreateProcessWithLogging' logLevel cp "")

testBuildUsingFlake :: (
  HasCallStack, MonadUnliftIO m, MonadLogger m, MonadReader context m
  , HasBaseContext context, HasBootstrapNixpkgs context
  ) => String -> m FilePath
testBuildUsingFlake expr = do
  rootDir <- findFirstParentMatching (\x -> doesPathExist (x </> ".git"))
  let cp = (proc "nix" ["build", expr, "--no-link", "--json"]) {
        cwd = Just rootDir
        , std_err = CreatePipe
        }
  out <- readCreateProcessWithLogging cp ""
  case parseNixBuildJson <$> (A.eitherDecode (BL8.pack out)) of
    Left err -> expectationFailure [i|Failed to parse bin paths: #{err}. JSON: #{out}|]
    Right Nothing -> expectationFailure [i|Didn't find Nix output in: #{out}|]
    Right (Just output) -> pure $ T.unpack output

testEval :: (
  HasCallStack, MonadUnliftIO m, MonadLogger m, MonadReader context m
  , HasBaseContext context, HasBootstrapNixpkgs context
  ) => String -> m ()
testEval expr = do
  rootDir <- findFirstParentMatching (\x -> doesPathExist (x </> ".git"))
  env <- getEnvWithNixPath

  p <- createProcessWithLogging $ (proc "nix-instantiate" ["--eval", "--strict", ".", "-A", expr, "--json"]) {
    cwd = Just rootDir
    , env = Just env
    }
  waitForProcess p >>= (`shouldBe` ExitSuccess)


getEnvWithNixPath :: (
  MonadUnliftIO m, MonadReader context m
  , HasBaseContext context, HasBootstrapNixpkgs context
  ) => m [(String, String)]
getEnvWithNixPath = do
  nixpkgsPath <- getContext bootstrapNixpkgs
  baseEnv <- getEnvironment
  return $ baseEnv
         & (("NIX_PATH", "nixpkgs=" <> nixpkgsPath) :)
         & L.nubBy (\x y -> fst x == fst y)
