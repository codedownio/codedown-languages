
module TestLib.NixEnvironmentContext where

import Control.Monad
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Aeson as A
import Data.ByteString.Lazy.Char8 as BL hiding (writeFile)
import qualified Data.HashMap.Strict as HM
import Data.String.Interpolate
import Data.Text as T
import System.Directory (getSymbolicLinkTarget)
import System.Exit
import System.FilePath
import Test.Sandwich
import TestLib.Aeson
import TestLib.NixRendering
import TestLib.NixTypes
import TestLib.Types
import TestLib.Util
import UnliftIO.Directory
import UnliftIO.IO
import UnliftIO.Process
import UnliftIO.Temporary


introduceNixEnvironment :: (
  HasBaseContext context, MonadIO m, MonadMask m, MonadUnliftIO m, MonadBaseControl IO m
  ) => [NixKernelSpec] -> [ChannelAndAttr] -> Text -> SpecFree (LabelValue "nixEnvironment" FilePath :> context) m () -> SpecFree context m ()
introduceNixEnvironment kernels otherPackages label  = introduceWith [i|#{label} Nix environment|] nixEnvironment $ \action -> do
  rootDir <- findFirstParentMatching (\x -> doesPathExist (x </> ".git"))

  metadata :: A.Object <- withFile "/dev/null" WriteMode $ \devNullHandle -> do
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
        nixEnvironmentMetaOnly = Nothing
        , nixEnvironmentChannels = [
            lockedToNixSrcSpec "nixpkgs" nixpkgsLocked
            , NixSrcSpec "codedown" [i|path:#{rootDir}|] NixSrcTypeFlake
            ]
        , nixEnvironmentOverlays = []
        , nixEnvironmentKernels = kernels
        , nixEnvironmentOtherPackages = otherPackages
        }

  let rendered = renderNixEnvironmentFlake nixEnv

  debug [i|Rendered: #{rendered}|]

  built <- withSystemTempDirectory "test-nix-build" $ \tmpDir -> do
    let linkPath = tmpDir </> "link"

    p <- do
      liftIO $ writeFile (tmpDir </> "flake.nix") (T.unpack rendered)
      createProcessWithLogging ((proc "nix" ["build"
                                            , "-o", linkPath]) { cwd = Just tmpDir })

    waitForProcess p >>= (`shouldBe` ExitSuccess)

    liftIO $ getSymbolicLinkTarget linkPath

  info [i|Built environment: #{built}|]

  void $ action built

parseNixpkgsSource :: A.Object -> Maybe Locked
parseNixpkgsSource (HM.lookup "locks" ->
                     Just (A.Object (HM.lookup "nodes" ->
                                      Just (A.Object (HM.lookup "nixpkgs" ->
                                                       Just (A.Object (HM.lookup "locked" -> Just (A.fromJSON -> A.Success (x :: Locked))))
                                                     ))
                                    ))
                   ) = Just x
parseNixpkgsSource _ =  Nothing
