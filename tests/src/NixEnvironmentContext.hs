{-# LANGUAGE ViewPatterns #-}

module NixEnvironmentContext where

import Control.Monad
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Unlift
import qualified Data.Aeson as A
import Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as HM
import Data.String.Interpolate
import Data.Text
import System.FilePath
import Test.Sandwich
import Types
import UnliftIO.Directory
import UnliftIO.Process


nixEnvironment :: Label "nixEnvironment" FilePath
nixEnvironment = Label
type HasNixEnvironment context = HasLabel context "nixEnvironment" FilePath

data NixEnvironment = NixEnvironment

introduceNixEnvironment :: (
  HasBaseContext context, MonadIO m, MonadMask m, MonadUnliftIO m
  ) => NixEnvironment -> Text -> SpecFree (LabelValue "nixEnvironment" FilePath :> context) m () -> SpecFree context m ()
introduceNixEnvironment nixEnv label  = introduceWith [i|#{label} Nix environment|] nixEnvironment $ \action -> do
  rootDir <- findFirstParentMatching (\x -> doesPathExist (x </> ".git"))

  metadata :: A.Object <- ((A.eitherDecode . BL.pack) <$>) (readCreateProcess ((proc "nix" ["flake", "metadata", "--json"]) { cwd = Just rootDir }) "") >>= \case
    Left err -> expectationFailure [i|Failed to parse flake metadata: #{err}|]
    Right x -> return x
  debug [i|Got full metadata: #{metadata}|]

  nixpkgsLocked <- case parseNixpkgsSource metadata of
    Nothing -> expectationFailure [i|Couldn't find nixpkgs lock info|]
    Just locked -> return locked
  info [i|Got locked: #{nixpkgsLocked}|]

  void $ action rootDir

parseNixpkgsSource (HM.lookup "locks" ->
                     Just (A.Object (HM.lookup "nodes" ->
                                      Just (A.Object (HM.lookup "nixpkgs" ->
                                                       Just (A.Object (HM.lookup "locked" -> Just (A.fromJSON -> A.Success (x :: Locked))))
                                                     ))
                                    ))
                   ) = return (Just x)
parseNixpkgsSource _ = return Nothing

findFirstParentMatching :: (MonadIO m, MonadThrow m) => (FilePath -> m Bool) -> m FilePath
findFirstParentMatching pred = getCurrentDirectory >>= findFirstParentMatching' pred

findFirstParentMatching' :: (MonadIO m, MonadThrow m) => (FilePath -> m Bool) -> FilePath -> m FilePath
findFirstParentMatching' pred startingAt = pred startingAt >>= \case
  True -> return startingAt
  False -> case takeDirectory startingAt of
    parent | parent /= startingAt -> findFirstParentMatching' pred parent
    parent -> expectationFailure [i|Couldn't find parent folder (could no longer traverse up at '#{parent}')|]
