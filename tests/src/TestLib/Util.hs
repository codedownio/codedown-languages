{-# LANGUAGE CPP #-}

module TestLib.Util where

import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Unlift
import Control.Retry
import Data.Aeson (Value)
import Data.String.Interpolate
import Data.Text as T
import Data.Time
import Data.Typeable
import GHC.Stack
import System.FilePath
import System.Timeout (Timeout)
import Test.Sandwich
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.Timeout

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

#if MIN_VERSION_aeson(2,0,0)
aesonFromList :: [(Text, Value)] -> HM.KeyMap Value
aesonFromList xs = HM.fromList [(A.fromText k, v) | (k, v) <- xs]
#else
aesonFromList :: (Eq k, Hashable k) => [(Text, Value)] -> HM.HashMap A.Key v
aesonFromList = HM.fromList
#endif

-- waitUntil :: forall m a. (HasCallStack, MonadIO m, MonadMask m, MonadThrow m) => Double -> m a -> m a
-- waitUntil timeInSeconds action = do
--   let policy = limitRetriesByCumulativeDelay (round (timeInSeconds * 1_000_000.0)) $ capDelay 200_000 $ exponentialBackoff 1_000
--   recoverAll policy $ const action

waitUntil :: forall m a. (HasCallStack, MonadIO m, MonadMask m, MonadThrow m, MonadUnliftIO m) => Double -> m a -> m a
waitUntil timeInSeconds action = do
  startTime <- liftIO getCurrentTime

  recoveringDynamic policy [handleFailureReasonException startTime] $ \_status ->
    rethrowTimeoutExceptionWithCallStack $
      timeout (round (timeInSeconds * 1_000_000)) action >>= \case
        Nothing -> throwIO $ userError [i|Action timed out in waitUntil|]
        Just x -> return x

  where
    policy = capDelay 1_000_000 $ exponentialBackoff 1_000

    handleFailureReasonException startTime _status = Handler $ \(_ :: SomeException) ->
      retryUnlessTimedOut startTime

    retryUnlessTimedOut :: UTCTime -> m RetryAction
    retryUnlessTimedOut startTime = do
      now <- liftIO getCurrentTime
      let thresh = secondsToNominalDiffTime (realToFrac timeInSeconds)
      if | (diffUTCTime now startTime) > thresh -> return DontRetry
         | otherwise -> return ConsultPolicy

    rethrowTimeoutExceptionWithCallStack :: (HasCallStack) => m a -> m a
    rethrowTimeoutExceptionWithCallStack = handleSyncOrAsync $ \(e@(SomeException inner)) ->
      if | Just (_ :: Timeout) <- fromExceptionUnwrap e -> do
             throwIO $ userError "Timeout in waitUntil"
         | Just (SyncExceptionWrapper (cast -> Just (SomeException (cast -> Just (SomeAsyncException (cast -> Just (_ :: Timeout))))))) <- cast inner -> do
             throwIO $ userError "Timeout in waitUntil"
         | otherwise -> do
             throwIO e
