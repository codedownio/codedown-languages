{-# LANGUAGE GADTs #-}

module TestLib.Contexts.PostgresqlDatabase (
  PostgresDatabaseTestContext (..)
  , withPostgresDatabase

  , createPostgresDatabase
  , waitForPostgresDatabase
  ) where

import Control.Monad
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.String.Interpolate
import Data.Text as T
import GHC.Stack
import Network.Socket (PortNumber)
import Safe
import System.Exit
import System.Process
import qualified System.Random as R
import Test.Sandwich
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Exception

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
  let cmd = [i|docker run -e POSTGRES_PASSWORD=#{password}
                          -e POSTGRES_USER=#{username}
                          -p 5432
                          #{T.unwords labelArgs}
                          --network #{networkName}
                          --health-cmd='pg_isready -U #{username}'
                          --health-interval=100ms
                          --name #{containerName}
                          -d postgres:13|]

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

  -- TODO: might be a good idea to do this here, rather than wrap a retry around the initial migrate later on
  -- waitForSimpleQuery pdtc

  return pdtc


-- withJoinOwnContainerToNetwork :: (
--   HasCallStack, MonadLoggerIO m, MonadBaseControl IO m, MonadUnliftIO m, MonadCatch m
--   ) => DockerState -> Text -> m a -> m a
-- withJoinOwnContainerToNetwork ds networkName action = do
--   maybeOwnContainerID <- liftIO getOwnContainerID >>= \case
--     Left err -> throwIO $ CodeDownException [i|Couldn't get own container ID: #{err}|] (Just callStack)
--     Right x-> return x
--   case maybeOwnContainerID of
--     Just ownContainerID -> do
--       bracket_ (joinContainerNetwork ds ownContainerID networkName)
--                (leaveContainerNetwork ds ownContainerID networkName)
--                action
--     Nothing -> action


-- waitForSimpleQuery :: forall m. (HasCallStack, MonadLoggerIO m, MonadUnliftIO m, MonadBaseControl IO m) => PostgresDatabaseTestContext -> m ()
-- waitForSimpleQuery pdtc@(PostgresDatabaseTestContext {..}) = undefined

-- test :: IO ()
-- test = runStdoutLoggingT $ do
--   withPostgresDatabase $ \db -> do
--     info [i|Got Postgres context: #{db}|]
--     void $ liftIO $ BS8.hGetLine stdin

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

uuidLetters :: [Char]
uuidLetters = ['a'..'z'] ++ ['0'..'9']

numUUIDLetters :: Int
numUUIDLetters = L.length uuidLetters

makeUUID' :: MonadIO m => Int -> m Text
makeUUID' n = T.pack <$> (replicateM n ((uuidLetters L.!!) <$> R.randomRIO (0, numUUIDLetters - 1)))
