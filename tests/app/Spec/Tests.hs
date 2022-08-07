{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -F -pgmF sandwich-discover -optF --module-prefix=Spec. #-}

module Spec.Tests where

import Control.Concurrent.QSem
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import Options.Applicative
import Test.Sandwich

#insert_test_imports


tests :: TopSpecWithOptions' SpecialOptions
tests = introduce "Introduce parallel semaphore" parallelSemaphore getQSem (const $ return ()) $ $(getSpecFromFolder $ defaultGetSpecFromFolderOptions {
  getSpecCombiner = 'describeParallel
  , getSpecIndividualSpecHooks = 'withParallelSemaphore
  , getSpecWarnOnParseError = NoWarnOnParseError
  })
  where
    getQSem = getCommandLineOptions >>= liftIO . newQSem . getParallelism


-- * Parallelism stuff

getParallelism :: CommandLineOptions SpecialOptions -> Int
getParallelism = optTestParallelism . optUserOptions

data SpecialOptions = SpecialOptions {
  optTestParallelism :: Int
  }

specialOptions :: Parser SpecialOptions
specialOptions = SpecialOptions
  <$> option auto (long "test-parallelism" <> short 'n' <> showDefault <> help "Test parallelism" <> value 4 <> metavar "INT")


describeParallel :: (
  MonadBaseControl IO m, MonadIO m, MonadMask m, HasParallelSemaphore context
  ) => String -> SpecFree context m () -> SpecFree context m ()
describeParallel s = (describe' (defaultNodeOptions { nodeOptionsRecordTime = False
                                                   , nodeOptionsVisibilityThreshold = 50 })) s
                   . (parallel' (defaultNodeOptions { nodeOptionsRecordTime = False
                                                    , nodeOptionsVisibilityThreshold = 125 }))

withParallelSemaphore :: forall context m. (
  MonadBaseControl IO m, MonadIO m, MonadMask m, HasParallelSemaphore context
  ) => FilePath -> SpecFree context m () -> SpecFree context m ()
withParallelSemaphore _ = around' (defaultNodeOptions { nodeOptionsRecordTime = False, nodeOptionsVisibilityThreshold = 125 }) "claim semaphore" $ \action -> do
  s <- getContext parallelSemaphore
  bracket_ (liftIO $ waitQSem s) (liftIO $ signalQSem s) (void action)

    -- $(getSpecFromFolder defaultGetSpecFromFolderOptions)
