
module TestLib.JupyterRunnerContext where

import Control.Monad
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Aeson as A
import Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as HM
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Vector as V
import System.Directory (getSymbolicLinkTarget)
import System.Exit
import System.FilePath
import Test.Sandwich
import TestLib.Aeson
import TestLib.NixRendering
import TestLib.NixTypes
import TestLib.Types
import TestLib.Util
import UnliftIO.Directory
import UnliftIO.Process
import UnliftIO.Temporary


introduceJupyterRunner :: (
  HasBaseContext context, MonadIO m, MonadMask m, MonadUnliftIO m, MonadBaseControl IO m
  ) => SpecFree (LabelValue "jupyterRunner" FilePath :> context) m () -> SpecFree context m ()
introduceJupyterRunner = introduceWith [i|Jupyter runner|] jupyterRunner $ \action -> do
  rootDir <- findFirstParentMatching (\x -> doesPathExist (x </> ".git"))

  let cp = ((proc "nix" ["build", ".#jupyter-runner", "--json"]) { cwd = Just rootDir })
  jupyterRunnerRaw :: A.Array <- ((A.eitherDecode . BL.pack) <$>) (readCreateProcess cp "") >>= \case
    Left err -> expectationFailure [i|Failed to parse build JSON: #{err}|]
    Right x -> return x

  case parseJson jupyterRunnerRaw of
    Nothing -> expectationFailure [i|Couldn't parse build path|]
    Just path -> void $ action $ T.unpack path

parseJson ((flip (V.!?) 0) -> Just (A.Object (HM.lookup "outputs" -> Just (A.Object (HM.lookup "out" -> Just (A.String t)))))) = Just t
parseJson _ = Nothing

testKernelStdout :: (HasJupyterRunner context, MonadIO m, MonadThrow m) => Text -> Text -> Text -> SpecFree context m ()
testKernelStdout kernel code desired = it [i|#{kernel}: #{code} -> #{desired}|] $ do
  jr <- getContext jupyterRunner
  (T.pack <$> readCreateProcess (proc jr ["run", "--kernel", T.unpack kernel]) (T.unpack code))
    >>= (`shouldBe` desired)
