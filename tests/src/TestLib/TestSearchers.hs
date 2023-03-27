
module TestLib.TestSearchers where

import Conduit as C
import Control.Monad
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Aeson as A
import Data.ByteString.Lazy.Char8 as BL
import Data.Conduit.Aeson as C
import qualified Data.HashMap.Strict as HM
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified System.Directory as SD
import System.Exit
import System.FilePath
import Test.Sandwich
import TestLib.Aeson
import TestLib.NixRendering
import TestLib.NixTypes
import TestLib.Types
import TestLib.Util
import UnliftIO.Directory
import UnliftIO.IO
import UnliftIO.Process


testKernelSearchers :: (
  HasBaseContext context, MonadIO m, MonadMask m, MonadUnliftIO m, MonadBaseControl IO m
  ) => Text -> SpecFree context m ()
testKernelSearchers kernel = it [i|#{kernel}: package and LSP searchers build|] $ testKernelSearchers' kernel

testKernelSearchers' :: (
  HasBaseContext context, MonadIO m, MonadMask m, MonadUnliftIO m, MonadBaseControl IO m
  ) => Text -> ExampleT context m ()
testKernelSearchers' kernel = do
  testBuild [i|.\#languages."#{kernel}".packageSearch|]
  testBuild [i|.\#languages."#{kernel}".languageServerSearch|]

testBuild :: (MonadIO m, MonadThrow m, MonadBaseControl IO m, MonadLogger m) => String -> m ()
testBuild expr = do
  rootDir <- findFirstParentMatching (\x -> doesPathExist (x </> ".git"))

  p <- createProcessWithLogging $ (proc "nix" ["build", expr, "--json", "--no-link"]) {
    cwd = Just rootDir
    }
  waitForProcess p >>= (`shouldBe` ExitSuccess)

testSearcherHasNonemptyResults :: (MonadUnliftIO m, MonadThrow m, MonadLogger m, MonadFail m) => String -> m ()
testSearcherHasNonemptyResults expr = searcherResults expr >>= \case
  xs | Prelude.length xs > 0 -> return ()
  x -> expectationFailure [i|Expected searcher to output an array with >=1 elem, but got #{x}|]

searcherResults :: (MonadUnliftIO m, MonadThrow m, MonadLogger m, MonadFail m) => String -> m [A.Object]
searcherResults expr = do
  rootDir <- findFirstParentMatching (\x -> doesPathExist (x </> ".git"))

  let cp = (proc "nix" ["run", expr]) {
        cwd = Just rootDir
        , std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        , create_group = True
        , close_fds = True
        }

  withCreateProcess cp $ \(Just hin) (Just hout) (Just herr) p -> do
    liftIO $ T.hPutStrLn hin "50"
    liftIO $ T.hPutStrLn hin "0"
    liftIO $ T.hPutStrLn hin ""
    liftIO $ hFlush hin

    runConduit $ sourceHandle hout .| C.conduitArray .| sinkList
