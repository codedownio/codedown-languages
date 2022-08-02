{-# LANGUAGE GADTs #-}

module TestLib.Contexts.PostgresqlDatabase (
  introducePostgres
  , introducePostgres'
  , postgresDb
  , HasPostgresDb

  , PostgresDatabaseTestContext (..)
  , withPostgresDatabase

  , createPostgresDatabase
  , waitForPostgresDatabase

  , postgresConnString
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
import qualified Data.Text.Encoding as T
import qualified Database.PostgreSQL.LibPQ as LPQ
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
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Exception


-- * Labels

postgresDb :: Label "postgresDb" (Pool PGS.Connection, PostgresDatabaseTestContext)
postgresDb = Label

type HasPostgresDb context = HasLabel context "postgresDb" (Pool PGS.Connection, PostgresDatabaseTestContext)

-- * Introduce functions

introducePostgres :: (
  HasBaseContext context, MonadMask m, MonadBaseControl IO m, MonadUnliftIO m
  ) => Maybe Text -> SpecFree (LabelValue "postgresDb" (Pool PGS.Connection, PostgresDatabaseTestContext) :> context) m () -> SpecFree context m ()
introducePostgres = introducePostgres' mempty

introducePostgres' :: (MonadMask m, MonadBaseControl IO m, MonadUnliftIO m, HasBaseContext context)
  => Map Text Text -> Maybe Text -> SpecFree (LabelValue "postgresDb" (Pool PGS.Connection, PostgresDatabaseTestContext) :> context) m () -> SpecFree context m ()
introducePostgres' labels maybeContainerName = introduceWith "Postgres database" postgresDb $ \cb ->
  withPostgresDatabase labels maybeContainerName $ \ctx@(PostgresDatabaseTestContext {..}) -> do
    let databaseConfig = PostgresDatabaseConfig {
          databaseHostname = postgresDatabaseLocalHostname
          , databasePort = postgresDatabaseLocalPort
          , databaseUsername = postgresDatabaseUsername
          , databasePassword = postgresDatabasePassword
          , databaseDatabase = postgresDatabaseDatabase
          }

    -- Retry until we connect successfully, since apparently the Docker healthcheck using pg_isready isn't enough
    let policy = constantDelay 50000 <> limitRetries 50
    _ <- recoverAll policy (\_ -> liftIO $ PGS.connectPostgreSQL (T.encodeUtf8 $ postgresConnString ctx))

    withSqlPool 5 databaseConfig $ \(pool, _) ->
      void $ cb (pool, ctx)

-- * Implementation

withPostgresDatabase :: (
  HasCallStack, MonadUnliftIO m, MonadBaseControl IO m, MonadLoggerIO m, MonadMask m, MonadReader context m, HasBaseContext context
  ) => Map Text Text -> Maybe Text -> (PostgresDatabaseTestContext -> m a) -> m a
withPostgresDatabase labels maybeContainerName action = do
  -- ds <- getDockerState False

  bracket (createPostgresDatabase labels maybeContainerName)
          (\(_, _, _, _, _, _containerID, containerName, networkName) -> timeAction "cleanup Postgres database" $ do
              info [i|Doing docker rm -f #{containerName}|]
              void $ liftIO $ readCreateProcess (shell [i|docker rm -f #{containerName}|]) ""
              -- void $ removeNetwork ds networkName
          )
          (\args@(_, _, _, _, _, _, _, networkName) -> do
              ctx <- waitForPostgresDatabase args
              -- testInContainer <- liftIO isInContainer
              -- let wrapper = if testInContainer then withJoinOwnContainerToNetwork ds networkName else id
              -- flip finally (removeNetwork ds networkName) $
              id (action ctx)
          )

createPostgresDatabase :: (
  HasCallStack, MonadLoggerIO m, MonadUnliftIO m, MonadBaseControl IO m, MonadThrow m, MonadMask m, MonadReader context m, HasBaseContext context
  ) => Map Text Text -> Maybe Text -> m (Text, PortNumber, Text, Text, Text, Text, Text, Text)
createPostgresDatabase labels maybeContainerName = timeAction "create Postgres database" $ do
  containerName <- maybe (("postgres-" <>) <$> makeUUID' 8) return maybeContainerName

  let localHostname = "127.0.0.1"
  let networkName = containerName <> "-network"
  liftIO $ void $ readCreateProcess (proc "docker" ["network", "create", T.unpack networkName]) ""

  let password = "foo"
  let database = "test"
  let username = "test"
  -- TODO: use shell escape on this
  let labelArgs = [[i|-l #{k}=#{v}|] | (k, v) <- M.toList labels]
  let cmd = [iii|docker run -e POSTGRES_PASSWORD=#{password}
                            -e POSTGRES_USER=#{username}
                            -p 5432
                            #{T.unwords labelArgs}
                            --network #{networkName}
                            --health-cmd='pg_isready -U #{username}'
                            --health-interval=100ms
                            --name #{containerName}
                            -d postgres:13|]

  info [i|cmd: #{cmd}|]
  (exitCode, sout, serr) <- liftIO $ readCreateProcessWithExitCode (shell $ T.unpack $ replace "\n" " " cmd) ""

  containerID <- case exitCode of
    ExitSuccess -> return $ strip $ T.pack sout
    _ -> expectationFailure [i|Failed to start Postgres container. Stdout: '#{sout}'. Stderr: '#{serr}'|]

  return (localHostname, 5432, username, password, database, containerID, containerName, networkName)

waitForPostgresDatabase :: (
  MonadUnliftIO m, MonadBaseControl IO m, MonadLoggerIO m, MonadThrow m
  ) => (Text, PortNumber, Text, Text, Text, Text, Text, Text) -> m PostgresDatabaseTestContext
waitForPostgresDatabase (localHostname, dockerPort, username, password, database, containerID, containerName, networkName) = do
  rawPort <- (strip . T.pack) <$> (liftIO $ readCreateProcess (shell [i|docker inspect --format='{{index .NetworkSettings.Ports "5432/tcp" 0 "HostPort"}}' #{containerName}|]) "")
  let localPort = fromMaybe (error [i|Couldn't read Docker port number: '#{rawPort}'|]) (readMay $ T.unpack rawPort)

  waitForHealth containerID

  let pdtc = PostgresDatabaseTestContext {
        postgresDatabaseLocalHostname = localHostname
        , postgresDatabaseLocalPort = localPort
        , postgresDatabaseUsername = username
        , postgresDatabasePassword = password
        , postgresDatabaseDatabase = database
        , postgresDatabaseDockerPort = dockerPort
        , postgresDatabaseDockerNetwork = networkName
        , postgresDatabaseContainerName = containerName
        }

  -- waitForSimpleQuery pdtc

  return pdtc


data PostgresDatabaseTestContext = PostgresDatabaseTestContext {
  postgresDatabaseLocalHostname :: Text
  , postgresDatabaseLocalPort :: PortNumber
  , postgresDatabaseUsername :: Text
  , postgresDatabasePassword :: Text
  , postgresDatabaseDatabase :: Text
  , postgresDatabaseDockerPort :: PortNumber
  , postgresDatabaseDockerNetwork :: Text
  , postgresDatabaseContainerName :: Text }
  deriving (Show, Eq)

data PostgresDatabaseConfig = PostgresDatabaseConfig {
  databaseHostname :: Text
  , databasePort :: PortNumber
  , databaseUsername :: Text
  , databasePassword :: Text
  , databaseDatabase :: Text
  } deriving (Show, Eq)

waitForHealth :: forall m. (HasCallStack, MonadLoggerIO m, MonadUnliftIO m, MonadBaseControl IO m, MonadThrow m) => Text -> m ()
waitForHealth containerID = race (liftIO $ threadDelay 60_000_000) waitForHealthLoop >>= \case
  Left () -> do
    info [i|Failed to wait for container #{containerID} to be ready|]
    expectationFailure [i|Failed to wait for container to be ready|]
  Right () -> return ()
  where
    waitForHealthLoop :: m ()
    waitForHealthLoop = fix $ \loop -> do
      health <- (strip . T.pack) <$> (liftIO $ readCreateProcess (shell [i|docker inspect --format "{{json .State.Health.Status }}" #{containerID}|]) "")
      case health of
       "\"healthy\"" -> return ()
       _ -> liftIO (threadDelay 100_000) >> loop

postgresConnString :: PostgresDatabaseTestContext -> Text
postgresConnString (PostgresDatabaseTestContext {..}) = [i|postgresql://#{postgresDatabaseUsername}:#{postgresDatabasePassword}@#{postgresDatabaseLocalHostname}:#{postgresDatabaseLocalPort}/#{postgresDatabaseDatabase}|]

uuidLetters :: [Char]
uuidLetters = ['a'..'z'] ++ ['0'..'9']

numUUIDLetters :: Int
numUUIDLetters = L.length uuidLetters

makeUUID' :: MonadIO m => Int -> m Text
makeUUID' n = T.pack <$> (replicateM n ((uuidLetters L.!!) <$> R.randomRIO (0, numUUIDLetters - 1)))

withSqlPool :: forall m a. (MonadMask m, MonadIO m, MonadUnliftIO m) => Int -> PostgresDatabaseConfig -> ((Pool PGS.Connection, PostgresDatabaseConfig) -> m a) -> m a
withSqlPool n config action = do
  bracket (liftIO $ createPool (connectPostgres config) PGS.close 1 30 n)
          (liftIO . destroyAllResources)
          (\pool -> action (pool, config))

connectPostgres :: PostgresDatabaseConfig -> IO PGS.Connection
connectPostgres (PostgresDatabaseConfig {..}) = do
  let connString = [i|postgresql://#{databaseUsername}:#{unpack databasePassword}@#{databaseHostname}:#{databasePort}/#{databaseDatabase}|]
  conn <- PGS.connectPostgreSQL connString
  withMVar (PGS.connectionHandle conn) LPQ.disableNoticeReporting
  return conn
