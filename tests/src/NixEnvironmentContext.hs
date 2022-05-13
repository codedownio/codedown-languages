
module NixEnvironmentContext where

import Control.Monad
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Unlift
import Data.String.Interpolate
import Data.Text
import System.FilePath
import Test.Sandwich
import UnliftIO.Directory


nixEnvironment :: Label "nixEnvironment" FilePath
nixEnvironment = Label
type HasNixEnvironment context = HasLabel context "nixEnvironment" FilePath

data NixEnvironment = NixEnvironment

introduceNixEnvironment :: (
  HasBaseContext context, MonadIO m, MonadMask m, MonadUnliftIO m
  ) => NixEnvironment -> Text -> SpecFree (LabelValue "nixEnvironment" FilePath :> context) m () -> SpecFree context m ()
introduceNixEnvironment nixEnv label  = introduceWith [i|#{label} Nix environment|] nixEnvironment $ \action -> do
  rootDir <- findFirstParentMatching (\x -> doesPathExist (x </> ".git"))
  void $ action rootDir

findFirstParentMatching :: (MonadIO m, MonadThrow m) => (FilePath -> m Bool) -> m FilePath
findFirstParentMatching pred = getCurrentDirectory >>= findFirstParentMatching' pred

findFirstParentMatching' :: (MonadIO m, MonadThrow m) => (FilePath -> m Bool) -> FilePath -> m FilePath
findFirstParentMatching' pred startingAt = pred startingAt >>= \case
  True -> return startingAt
  False -> case takeDirectory startingAt of
    parent | parent /= startingAt -> findFirstParentMatching' pred parent
    parent -> expectationFailure [i|Couldn't find parent folder (could no longer traverse up at '#{parent}')|]
