
module TestLib.NixEnvironmentContext where

import Control.Monad
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Aeson as A
import Data.ByteString.Lazy.Char8 as BL
import Data.String.Interpolate
import Data.Text as T
import qualified System.Directory as SD
import System.Exit
import System.FilePath
import Test.Sandwich
import TestLib.NixRendering
import TestLib.NixTypes
import TestLib.Types
import TestLib.Util
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.IO
import UnliftIO.Process
import UnliftIO.Temporary


introduceNixEnvironment :: (
  HasBaseContext context, MonadIO m, MonadMask m, MonadUnliftIO m, MonadBaseControl IO m
  )
  => [NixKernelSpec]
  -> [Text]
  -> Text
  -> SpecFree (LabelValue "nixEnvironment" FilePath :> context) m ()
  -> SpecFree context m ()
introduceNixEnvironment kernels otherConfig label = introduceWith' (defaultNodeOptions {nodeOptionsVisibilityThreshold = 50}) [i|#{label}|] nixEnvironment $ \action -> do
  rootDir <- findFirstParentMatching (\x -> doesPathExist (x </> ".git"))

  metadata :: A.Object <- bracket (openFile "/dev/null" WriteMode) hClose $ \devNullHandle -> do
    let cp = (proc "nix" ["flake", "metadata", "--json"]) {
          cwd = Just rootDir
          , std_err = UseHandle devNullHandle
          }

    (A.eitherDecode . BL.pack) <$> (readCreateProcess cp "") >>= \case
      Left err -> expectationFailure [i|Failed to parse flake metadata: #{err}|]
      Right x -> return x

  nixpkgsLocked <- case parseNixpkgsSource metadata of
    Nothing -> expectationFailure [i|Couldn't find nixpkgs lock info|]
    Just locked -> return locked

  let nixEnv = NixEnvironment {
        nixEnvironmentChannels = [
            NixSrcPath "codedown" (T.pack rootDir)
            , lockedToNixSrcSpec "nixpkgs" nixpkgsLocked
            ]
        , nixEnvironmentKernels = kernels
        , nixEnvironmentOtherConfig = otherConfig
        }

  let rendered = renderNixEnvironment "<nixpkgs>" nixEnv
  debug [i|nixEnv: #{nixEnv}|]
  debug [i|Rendered: #{rendered}|]

  built <- withSystemTempDirectory "test-nix-build" $ \((</> "link") -> linkPath) -> do
    createProcessWithLogging (proc "nix-build" ["--quiet", "--no-out-link"
                                               , "-E", T.unpack rendered
                                               , "-o", linkPath
                                               ])
      >>= waitForProcess >>= (`shouldBe` ExitSuccess)
    liftIO $ SD.getSymbolicLinkTarget linkPath

  void $ action built

parseNixpkgsSource :: A.Object -> Maybe Locked
parseNixpkgsSource (aesonLookup "locks" ->
                     Just (A.Object (aesonLookup "nodes" ->
                                      Just (A.Object (aesonLookup "nixpkgs" ->
                                                       Just (A.Object (aesonLookup "locked" -> Just (A.fromJSON -> A.Success (x :: Locked))))
                                                     ))
                                    ))
                   ) = Just x
parseNixpkgsSource _ =  Nothing
