{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -F -pgmF sandwich-discover -optF --module-prefix=Spec. #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Spec.Tests where

import Control.Concurrent.QSem
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Typeable
import Control.Monad.Trans.Control (MonadBaseControl)
import Options.Applicative hiding (action)
import Test.Sandwich
import TestLib.Types
import TestLib.JupyterRunnerContext

#insert_test_imports


tests :: forall context. (
  HasBaseContext context
  , HasCommandLineOptions context SpecialOptions
  , Typeable context
  ) => SpecFree context IO ()
tests =
  introduceJupyterRunner $
    introduce' (defaultNodeOptions { nodeOptionsCreateFolder = False }) "Introduce parallel semaphore" parallelSemaphore getQSem (const $ return ()) $
      $(getSpecFromFolder $ defaultGetSpecFromFolderOptions {
           getSpecCombiner = 'describeParallel
           , getSpecIndividualSpecHooks = 'withParallelSemaphore
           , getSpecWarnOnParseError = NoWarnOnParseError
           })
  where
    getQSem = getCommandLineOptions >>= liftIO . newQSem . getParallelism


-- * Parallelism stuff

getParallelism :: CommandLineOptions SpecialOptions -> Int
getParallelism = optTestParallelism . optUserOptions

describeParallel :: (
  MonadBaseControl IO m, MonadIO m, MonadMask m, HasParallelSemaphore context
  ) => String -> SpecFree context m () -> SpecFree context m ()
describeParallel s = (describe' (defaultNodeOptions { nodeOptionsRecordTime = False
                                                    , nodeOptionsVisibilityThreshold = 50 })) s
                   . (parallel' (defaultNodeOptions { nodeOptionsRecordTime = False
                                                    , nodeOptionsVisibilityThreshold = 125
                                                    , nodeOptionsCreateFolder = False }))

withParallelSemaphore :: forall context m. (
  MonadBaseControl IO m, MonadIO m, MonadMask m, HasParallelSemaphore context
  ) => FilePath -> SpecFree context m () -> SpecFree context m ()
withParallelSemaphore _ = around' (defaultNodeOptions { nodeOptionsRecordTime = False
                                                      , nodeOptionsVisibilityThreshold = 125
                                                      , nodeOptionsCreateFolder = False }) "claim semaphore" $ \action -> do
  s <- getContext parallelSemaphore
  bracket_ (liftIO $ waitQSem s) (liftIO $ signalQSem s) (void action)
