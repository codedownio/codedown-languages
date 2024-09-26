{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module TestLib.TestSearchers where

import Conduit as C
import Control.Monad
import Control.Monad.Catch (MonadMask)
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Aeson as A
import Data.Conduit.Aeson as C
import qualified Data.List as L
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Text.IO as T
import System.FilePath
import Test.Sandwich
import TestLib.TestBuilding
import TestLib.Types
import UnliftIO.IO
import UnliftIO.Process


-- Testing for successful build

testKernelSearchersBuild :: (
  MonadIO m, MonadMask m, MonadUnliftIO m, MonadBaseControl IO m
  , HasBaseContext context, HasBootstrapNixpkgs context
  ) => Text -> SpecFree context m ()
testKernelSearchersBuild kernel = it [i|#{kernel}: package searchers build|] $ do
  void $ testBuild [i|kernels."#{kernel}".packageSearch|]

testHasExpectedFields :: (
  MonadIO m, MonadMask m, MonadUnliftIO m, MonadBaseControl IO m
  , HasBaseContext context, HasBootstrapNixpkgs context
  ) => Text -> SpecFree context m ()
testHasExpectedFields kernel = it [i|#{kernel}: has expected fields|] $ do
  testEval [i|kernels."#{kernel}".settingsSchema|]
  testEval [i|kernels."#{kernel}".modes|]
  testEval [i|kernels."#{kernel}".settings|]
  testEval [i|kernels."#{kernel}".args|]
  testEval [i|kernels."#{kernel}".meta|]

  -- Used to view all versions in codedown-languages
  testEval [i|kernels."#{kernel}".versions|]

-- Testing for nonempty results

-- | A stronger version of testKernelSearchersBuild
testKernelSearchersNonempty :: (
  MonadIO m, MonadMask m, MonadUnliftIO m, MonadBaseControl IO m
  , HasBaseContext context, HasBootstrapNixpkgs context
  ) => Text -> SpecFree context m ()
testKernelSearchersNonempty kernel = describe [i|#{kernel}: package and LSP searchers build and have results|] $ do
  it "package searcher" $ testPackageSearchNonempty kernel

testPackageSearchNonempty :: (
  MonadIO m, MonadMask m, MonadUnliftIO m, MonadBaseControl IO m
  , HasBaseContext context, HasBootstrapNixpkgs context
  ) => Text -> ExampleT context m ()
testPackageSearchNonempty kernel = testSearcherHasNonemptyResults [i|kernels."#{kernel}".packageSearch|]

testSearcherHasNonemptyResults :: (
  MonadUnliftIO m, MonadThrow m, MonadLogger m, MonadFail m, MonadReader context m
  , HasBaseContext context, HasBootstrapNixpkgs context
  ) => String -> m ()
testSearcherHasNonemptyResults expr = searcherResults expr >>= \case
  xs | not (L.null xs) -> return ()
  x -> expectationFailure [i|Expected searcher to output an array with >=1 elem, but got #{x}|]

searcherResults :: (
  MonadUnliftIO m, MonadThrow m, MonadLogger m, MonadFail m, MonadReader context m
  , HasBaseContext context, HasBootstrapNixpkgs context
  ) => String -> m [A.Object]
searcherResults expr = do
  built <- testBuild expr
  info [i|Got built searcher: #{built}|]

  let cp' = (proc (built </> "bin" </> "searcher") []) {
        std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        , create_group = True
        , close_fds = True
        }

  withCreateProcess cp' $ \(Just hin) (Just hout) (Just _herr) _p -> do
    liftIO $ T.hPutStrLn hin "50"
    liftIO $ T.hPutStrLn hin "0"
    liftIO $ T.hPutStrLn hin ""
    liftIO $ hFlush hin

    runConduit $ sourceHandle hout .| C.conduitArray .| sinkList
