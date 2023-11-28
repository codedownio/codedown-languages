{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module TestLib.TestBuilding where

import Conduit as C
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)
import System.Exit
import System.FilePath
import Test.Sandwich
import TestLib.Util
import UnliftIO.Directory
import UnliftIO.Process


testBuild :: (MonadIO m, MonadThrow m, MonadBaseControl IO m, MonadLogger m) => String -> m ()
testBuild = testBuild' LevelDebug

testBuild' :: (MonadIO m, MonadThrow m, MonadBaseControl IO m, MonadLogger m) => LogLevel -> String -> m ()
testBuild' logLevel expr = do
  rootDir <- findFirstParentMatching (\x -> doesPathExist (x </> ".git"))

  p <- createProcessWithLogging' logLevel $ (proc "nix" ["build", expr, "--json", "--no-link"]) {
    cwd = Just rootDir
    }
  waitForProcess p >>= (`shouldBe` ExitSuccess)

testEval :: (MonadIO m, MonadThrow m, MonadBaseControl IO m, MonadLogger m) => String -> m ()
testEval expr = do
  rootDir <- findFirstParentMatching (\x -> doesPathExist (x </> ".git"))

  p <- createProcessWithLogging $ (proc "nix" ["eval", expr, "--json"]) {
    cwd = Just rootDir
    }
  waitForProcess p >>= (`shouldBe` ExitSuccess)
