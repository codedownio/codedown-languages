
module TestLib.TestSearchers where

import Control.Monad
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Aeson as A
import Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as HM
import Data.String.Interpolate
import Data.Text as T
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
import UnliftIO.Temporary


testKernelSearchers :: (
  HasBaseContext context, MonadIO m, MonadMask m, MonadUnliftIO m, MonadBaseControl IO m
  ) => Text -> SpecFree context m ()
testKernelSearchers kernel = it [i|#{kernel}: searchers build|] $ testKernelSearchers' kernel

testKernelSearchers' :: (
  HasBaseContext context, MonadIO m, MonadMask m, MonadUnliftIO m, MonadBaseControl IO m
  ) => Text -> ExampleT context m ()
testKernelSearchers' kernel = do
  testBuild [i|.\#languages."#{kernel}".packageSearch|]
  testBuild [i|.\#languages."#{kernel}".languageServerSearch|]

testBuild expr = do
  rootDir <- findFirstParentMatching (\x -> doesPathExist (x </> ".git"))

  withFile "/dev/null" WriteMode $ \devNullHandle -> do
    p <- createProcessWithLogging $ (proc "nix" ["build", expr, "--json"]) {
      cwd = Just rootDir
      , std_err = UseHandle devNullHandle
      }
    waitForProcess p >>= (`shouldBe` ExitSuccess)
