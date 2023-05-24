{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module TestLib.TestSearchers where

import Conduit as C
import Control.Monad.Catch (MonadMask)
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Conduit.Aeson as C
import qualified Data.List as L
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import System.Exit
import System.FilePath
import Test.Sandwich
import TestLib.Util
import UnliftIO.Directory
import UnliftIO.IO
import UnliftIO.Process


-- Testing for successful build

testKernelSearchersBuild :: (
  HasBaseContext context, MonadIO m, MonadMask m, MonadUnliftIO m, MonadBaseControl IO m
  ) => Text -> SpecFree context m ()
testKernelSearchersBuild kernel = it [i|#{kernel}: package searchers build|] $ do
  testPackageSearchBuild kernel

testPackageSearchBuild :: (
  HasBaseContext context, MonadIO m, MonadMask m, MonadUnliftIO m, MonadBaseControl IO m
  ) => Text -> ExampleT context m ()
testPackageSearchBuild kernel = testBuild [i|.\#languages."#{kernel}".packageSearch|]

testBuild :: (MonadIO m, MonadThrow m, MonadBaseControl IO m, MonadLogger m) => String -> m ()
testBuild expr = do
  rootDir <- findFirstParentMatching (\x -> doesPathExist (x </> ".git"))

  p <- createProcessWithLogging $ (proc "nix" ["build", expr, "--json", "--no-link"]) {
    cwd = Just rootDir
    }
  waitForProcess p >>= (`shouldBe` ExitSuccess)

-- Testing for nonempty results

-- | A stronger version of testKernelSearchersBuild
testKernelSearchersNonempty :: (
  HasBaseContext context, MonadIO m, MonadMask m, MonadUnliftIO m, MonadBaseControl IO m
  ) => Text -> SpecFree context m ()
testKernelSearchersNonempty kernel = describe [i|#{kernel}: package and LSP searchers build and have results|] $ do
  it "package searcher" $ testPackageSearchNonempty kernel

testPackageSearchNonempty :: (
  HasBaseContext context, MonadIO m, MonadMask m, MonadUnliftIO m, MonadBaseControl IO m
  ) => Text -> ExampleT context m ()
testPackageSearchNonempty kernel = testSearcherHasNonemptyResults [i|.\#languages."#{kernel}".packageSearch|]

testSearcherHasNonemptyResults :: (MonadUnliftIO m, MonadThrow m, MonadLogger m, MonadFail m) => String -> m ()
testSearcherHasNonemptyResults expr = searcherResults expr >>= \case
  xs | not (L.null xs) -> return ()
  x -> expectationFailure [i|Expected searcher to output an array with >=1 elem, but got #{x}|]

searcherResults :: (MonadUnliftIO m, MonadThrow m, MonadLogger m, MonadFail m) => String -> m [A.Object]
searcherResults expr = do
  rootDir <- findFirstParentMatching (\x -> doesPathExist (x </> ".git"))

  built <- ((A.eitherDecode . BL8.pack) <$> readCreateProcess ((proc "nix" ["build", expr, "--no-link", "--json"]) { cwd = Just rootDir, std_err = CreatePipe }) "") >>= \case
    Left err -> expectationFailure [i|Failed to decode JSON: #{err}|]
    Right (A.Array ((V.! 0) -> (A.Object (aesonLookup "outputs" -> Just (A.Object (aesonLookup "out" -> Just (A.String x))))))) -> pure x
    Right x -> expectationFailure [i|Unexpected JSON: #{x}|]

  let cp = (proc (T.unpack built) []) {
        cwd = Just rootDir
        , std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        , create_group = True
        , close_fds = True
        }

  withCreateProcess cp $ \(Just hin) (Just hout) (Just _herr) _p -> do
    liftIO $ T.hPutStrLn hin "50"
    liftIO $ T.hPutStrLn hin "0"
    liftIO $ T.hPutStrLn hin ""
    liftIO $ hFlush hin

    runConduit $ sourceHandle hout .| C.conduitArray .| sinkList
