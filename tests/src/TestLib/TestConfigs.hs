{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module TestLib.TestConfigs where

import Conduit as C
import Control.Monad.Catch (MonadMask)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.String.Interpolate
import Data.Text as T
import Test.Sandwich


-- Testing for successful build

testLanguageServerConfigs :: (
  HasBaseContext context, MonadIO m, MonadMask m, MonadUnliftIO m, MonadBaseControl IO m
  ) => Text -> SpecFree context m ()
testLanguageServerConfigs kernel = it [i|#{kernel}: language server configs look good|] $ do
  undefined
