{-# LANGUAGE GADTs #-}

module TestLib.Contexts.PostgresqlData (
  introducePostgresData
  ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.String.Interpolate
import Data.Text as T
import Data.Text.Encoding
import Database.PostgreSQL.Simple
import Test.Sandwich
import Test.Sandwich.Contexts.PostgreSQL
import UnliftIO.Exception


introducePostgresData :: (
  MonadMask m, MonadBaseControl IO m, MonadUnliftIO m
  , HasBaseContext context, HasLabel context "postgres" PostgresContext
  ) => SpecFree context m () -> SpecFree context m ()
introducePostgresData = before "Postgres data" $ do
  PostgresContext {..} <- getContext postgres
  liftIO $ bracket (connectPostgreSQL (encodeUtf8 postgresConnString)) close $ \conn -> do
    _ <- execute_ conn [i|create table test_table (name varchar(40), num smallint)|]

    affected <- executeMany conn "insert into test_table (name, num) values (?, ?)" [
      ("foo" :: Text, 1 :: Int)
      , ("bar", 2)
      , ("baz", 3)
      ]
    affected `shouldBe` 3
