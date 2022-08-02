{-# LANGUAGE GADTs #-}

module TestLib.Contexts.PostgresqlData (
  introducePostgresData
  ) where

import Control.Monad
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Retry
import qualified Data.ByteString.Char8 as BS8
import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Pool
import Data.String.Interpolate
import Data.Text as T
import qualified Database.PostgreSQL.LibPQ as LPQ
import Database.PostgreSQL.Simple
import qualified Database.PostgreSQL.Simple as PGS
import qualified Database.PostgreSQL.Simple.Internal as PGS
import GHC.Stack
import Network.Socket (PortNumber)
import Safe
import System.Exit
import System.IO
import System.Process
import qualified System.Random as R
import Test.Sandwich
import TestLib.Contexts.PostgresqlDatabase
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Exception


introducePostgresData :: (
  HasBaseContext context, MonadMask m, MonadBaseControl IO m, MonadUnliftIO m, HasPostgresDb context
  ) => SpecFree context m () -> SpecFree context m ()
introducePostgresData = before "Postgres data" $ do
  (pool, PostgresDatabaseTestContext {..}) <- getContext postgresDb
  withResource pool $ \conn -> liftIO $ do
    _ <- execute_ conn [i|create table test_table (name varchar(40), num smallint)|]

    affected <- executeMany conn "insert into test_table (name, num) values (?, ?)" [
      ("foo" :: Text, 1 :: Int)
      , ("bar", 2)
      , ("baz", 3)
      ]
    affected `shouldBe` 3
