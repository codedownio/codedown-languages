{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module TestLib.TestBuilding where

import Conduit as C
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Function
import qualified Data.List as L
import GHC.Stack
import System.Exit
import System.FilePath
import Test.Sandwich
import TestLib.Types
import TestLib.Util
import UnliftIO.Directory
import UnliftIO.Environment
import UnliftIO.Process


testBuild :: (
  HasCallStack, MonadUnliftIO m, MonadLogger m, MonadReader context m
  , HasBaseContext context, HasBootstrapNixpkgs context
  ) => String -> m ()
testBuild = testBuild' LevelDebug

testBuild' :: (
  HasCallStack, MonadUnliftIO m, MonadLogger m, MonadReader context m
  , HasBaseContext context, HasBootstrapNixpkgs context
  ) => LogLevel -> String -> m ()
testBuild' logLevel expr = do
  rootDir <- findFirstParentMatching (\x -> doesPathExist (x </> ".git"))
  env <- getEnvWithNixPath

  p <- createProcessWithLogging' logLevel $ (proc "nix-build" [".", "-A", expr, "--no-out-link"]) {
    cwd = Just rootDir
    , env = Just env
    }
  waitForProcess p >>= (`shouldBe` ExitSuccess)

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
