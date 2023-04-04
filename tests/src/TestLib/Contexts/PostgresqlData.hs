{-# LANGUAGE GADTs #-}

module TestLib.Contexts.PostgresqlData (
  introducePostgresData
  ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Pool
import Data.String.Interpolate
import Data.Text as T
import Database.PostgreSQL.Simple
import Test.Sandwich
import TestLib.Contexts.PostgresqlDatabase


introducePostgresData :: (
  HasBaseContext context, MonadMask m, MonadBaseControl IO m, MonadUnliftIO m, HasPostgresDb context
  ) => SpecFree context m () -> SpecFree context m ()
introducePostgresData = before "Postgres data" $ do
  (pool, PostgresDatabaseTestContext {}) <- getContext postgresDb
  withResource pool $ \conn -> liftIO $ do
    _ <- execute_ conn [i|create table test_table (name varchar(40), num smallint)|]

    affected <- executeMany conn "insert into test_table (name, num) values (?, ?)" [
      ("foo" :: Text, 1 :: Int)
      , ("bar", 2)
      , ("baz", 3)
      ]
    affected `shouldBe` 3
