{-# LANGUAGE ViewPatterns #-}

module TestLib.NixEnvironmentContext where

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
  ) => [NixKernelSpec] -> [ChannelAndAttr] -> Text -> SpecFree (LabelValue "nixEnvironment" FilePath :> context) m () -> SpecFree context m ()
introduceNixEnvironment kernels otherPackages label  = introduceWith [i|#{label} Nix environment|] nixEnvironment $ \action -> do
  rootDir <- findFirstParentMatching (\x -> doesPathExist (x </> ".git"))

  metadata :: A.Object <- ((A.eitherDecode . BL.pack) <$>) (readCreateProcess ((proc "nix" ["flake", "metadata", "--json"]) { cwd = Just rootDir }) "") >>= \case
    Left err -> expectationFailure [i|Failed to parse flake metadata: #{err}|]
    Right x -> return x

  nixpkgsLocked <- case parseNixpkgsSource metadata of
    Nothing -> expectationFailure [i|Couldn't find nixpkgs lock info|]
    Just locked -> return locked

  let nixEnv = NixEnvironment {
        nixEnvironmentMetaOnly = Nothing
        , nixEnvironmentChannels = [lockedToNixSrcSpec "nixpkgs" nixpkgsLocked]
        , nixEnvironmentOverlays = [NixSrcPath "codedown" (T.pack rootDir)]
        , nixEnvironmentKernels = kernels
        , nixEnvironmentOtherPackages = otherPackages
        }

  let rendered = renderNixEnvironment "<nixpkgs>" nixEnv
  debug [i|Rendered: #{rendered}|]

  built <- withSystemTempDirectory "test-nix-build" $ \((</> "link") -> linkPath) -> do
    createProcessWithLogging (proc "nix-build" ["--quiet", "--no-out-link"
                                               , "-E", T.unpack rendered
                                               , "-o", linkPath
                                               ])
      >>= waitForProcess >>= (`shouldBe` ExitSuccess)
    liftIO $ getSymbolicLinkTarget linkPath

  void $ action built

parseNixpkgsSource :: A.Object -> Maybe Locked
parseNixpkgsSource (HM.lookup "locks" ->
                     Just (A.Object (HM.lookup "nodes" ->
                                      Just (A.Object (HM.lookup "nixpkgs" ->
                                                       Just (A.Object (HM.lookup "locked" -> Just (A.fromJSON -> A.Success (x :: Locked))))
                                                     ))
                                    ))
                   ) = Just x
parseNixpkgsSource _ =  Nothing

findFirstParentMatching :: (MonadIO m, MonadThrow m) => (FilePath -> m Bool) -> m FilePath
findFirstParentMatching pred = getCurrentDirectory >>= findFirstParentMatching' pred

findFirstParentMatching' :: (MonadIO m, MonadThrow m) => (FilePath -> m Bool) -> FilePath -> m FilePath
findFirstParentMatching' pred startingAt = pred startingAt >>= \case
  True -> return startingAt
  False -> case takeDirectory startingAt of
    parent | parent /= startingAt -> findFirstParentMatching' pred parent
    parent -> expectationFailure [i|Couldn't find parent folder (could no longer traverse up at '#{parent}')|]
