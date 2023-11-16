{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module TestLib.TestConfigs where

import Conduit as C
import Control.Monad.Catch (MonadMask)
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Conduit.Aeson as C
import qualified Data.List as L
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import System.Exit
import System.FilePath
import Test.Sandwich
import TestLib.TestBuilding
import TestLib.Util
import UnliftIO.Directory
import UnliftIO.IO
import UnliftIO.Process


-- Testing for successful build

testLanguageServerConfigs :: (
  HasBaseContext context, MonadIO m, MonadMask m, MonadUnliftIO m, MonadBaseControl IO m
  ) => Text -> SpecFree context m ()
testLanguageServerConfigs kernel = it [i|#{kernel}: language server configs look good|] $ do
  undefined
