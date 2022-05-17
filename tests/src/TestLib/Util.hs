
module TestLib.Util where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Unlift
import Data.String.Interpolate
import System.FilePath
import Test.Sandwich
import UnliftIO.Directory


findFirstParentMatching :: (MonadIO m, MonadThrow m) => (FilePath -> m Bool) -> m FilePath
findFirstParentMatching pred = getCurrentDirectory >>= findFirstParentMatching' pred

findFirstParentMatching' :: (MonadIO m, MonadThrow m) => (FilePath -> m Bool) -> FilePath -> m FilePath
findFirstParentMatching' pred startingAt = pred startingAt >>= \case
  True -> return startingAt
  False -> case takeDirectory startingAt of
    parent | parent /= startingAt -> findFirstParentMatching' pred parent
    parent -> expectationFailure [i|Couldn't find parent folder (could no longer traverse up at '#{parent}')|]
