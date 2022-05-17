{-# LANGUAGE ViewPatterns #-}

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
import System.Directory (getSymbolicLinkTarget)
import System.Exit
import System.FilePath
import Test.Sandwich
import TestLib.Aeson
import TestLib.NixRendering
import TestLib.NixTypes
import TestLib.Types
import UnliftIO.Directory
import UnliftIO.Process
import UnliftIO.Temporary


introduceNixEnvironment :: (
  HasBaseContext context, MonadIO m, MonadMask m, MonadUnliftIO m, MonadBaseControl IO m
  ) => SpecFree (LabelValue "jupyterRunner" FilePath :> context) m () -> SpecFree context m ()
introduceJupyterRunner = introduceWith [i|Jupyter runner|] nixEnvironment $ \action -> do
  rootDir <- findFirstParentMatching (\x -> doesPathExist (x </> ".git"))

  metadata :: A.Object <- ((A.eitherDecode . BL.pack) <$>) (readCreateProcess ((proc "nix" ["flake", "metadata", "--json"]) { cwd = Just rootDir }) "") >>= \case
    Left err -> expectationFailure [i|Failed to parse flake metadata: #{err}|]
    Right x -> return x

  undefined
