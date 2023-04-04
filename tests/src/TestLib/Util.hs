{-# LANGUAGE CPP #-}

module TestLib.Util where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Unlift
import Data.String.Interpolate
import Data.Text as T
import System.FilePath
import Test.Sandwich
import UnliftIO.Directory

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key             as A
import qualified Data.Aeson.KeyMap          as HM
#else
import Data.Hashable
import qualified Data.HashMap.Strict        as HM
#endif


findFirstParentMatching :: (MonadIO m, MonadThrow m) => (FilePath -> m Bool) -> m FilePath
findFirstParentMatching cb = getCurrentDirectory >>= findFirstParentMatching' cb

findFirstParentMatching' :: (MonadIO m, MonadThrow m) => (FilePath -> m Bool) -> FilePath -> m FilePath
findFirstParentMatching' cb startingAt = cb startingAt >>= \case
  True -> return startingAt
  False -> case takeDirectory startingAt of
    parent | parent /= startingAt -> findFirstParentMatching' cb parent
    parent -> expectationFailure [i|Couldn't find parent folder (could no longer traverse up at '#{parent}')|]

#if MIN_VERSION_aeson(2,0,0)
aesonLookup :: Text -> HM.KeyMap v -> Maybe v
aesonLookup = HM.lookup . A.fromText
#else
aesonLookup :: (Eq k, Hashable k) => k -> HM.HashMap k v -> Maybe v
aesonLookup = HM.lookup
#endif
