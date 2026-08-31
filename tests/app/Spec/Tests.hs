{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -F -pgmF sandwich-discover -optF --module-prefix=Spec. #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Spec.Tests where

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
  introduceJustBubblewrap $
  introduceBootstrapNixpkgs $
    withParallelLanesFromArgs getParallelism $
      $(getSpecFromFolder $ defaultGetSpecFromFolderOptions {
           getSpecCombiner = 'describeParallel
           , getSpecIndividualSpecHooks = 'takeParallelLane
           })


-- * Parallelism stuff

getParallelism :: CommandLineOptions SpecialOptions -> Int
getParallelism = optTestParallelism . optUserOptions

describeParallel :: (
  MonadBaseControl IO m, MonadIO m, MonadMask m
  ) => String -> SpecFree context m () -> SpecFree context m ()
describeParallel s = (describe' (defaultNodeOptions { nodeOptionsRecordTime = False
                                                    , nodeOptionsVisibilityThreshold = 50 })) s
                   . (parallel' (defaultNodeOptions { nodeOptionsRecordTime = False
                                                    , nodeOptionsVisibilityThreshold = 125
                                                    , nodeOptionsCreateFolder = False }))


