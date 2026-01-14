
module TestLib.NixEnvironmentContext where

import Control.Monad
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Aeson as A
import Data.ByteString.Lazy.Char8 as BL
import qualified Data.List as L
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.URI.Encode as URI
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
introduceNixEnvironment kernels otherConfig label = introduceWith' (defaultNodeOptions {nodeOptionsVisibilityThreshold = 60}) [i|#{label}|] nixEnvironment $ \action -> do
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

  let nixpkgsSrcSpec = lockedToNixSrcSpec "nixpkgs" nixpkgsLocked

  let nixEnv = NixEnvironment {
        nixEnvironmentChannels = [
            NixSrcPath "codedown" [i|builtins.fetchTree { type = "path"; path = "#{rootDir}"; }|]
            , nixpkgsSrcSpec
            ]
        , nixEnvironmentKernels = kernels
        , nixEnvironmentOtherConfig = otherConfig
        }

  let rendered = renderNixEnvironment (renderNixSrcSpecBuiltins nixpkgsSrcSpec) nixEnv
  debug [i|nixEnv: #{nixEnv}|]

  Just dir <- getCurrentFolder
  liftIO $ T.writeFile (dir </> "expr.nix") rendered

  nixpkgsUri <- case nixpkgsSrcSpec of
    NixSrcFetchFromGithub {..} -> pure [i|github:#{nixSrcOwner}/#{nixSrcRepo}/#{nixSrcRev}?narHash=#{URI.encodeTextWith customIsAllowed nixSrcHash}|]
    x -> expectationFailure [i|Unhandled src spec type: #{x}|]

  built <- withTempDirectory dir "test-nix-build" $ \((</> "link") -> linkPath) -> do
    let args = ["build"
               , "--quiet"
               , "--no-link"
               , "--impure"
               -- , "--include", rootDir
               , "--option", "restrict-eval", "true"
               , "--option", "allowed-uris", L.unwords ["path:" <> rootDir, nixpkgsUri]
               , "--expr", T.unpack rendered
               , "-o", linkPath
               ]
    info [i|nix #{L.unwords args}|]
    createProcessWithLogging (proc "nix" args)
      >>= waitForProcess >>= (`shouldBe` ExitSuccess)
    liftIO $ SD.getSymbolicLinkTarget linkPath

  info [i|Built Nix environment: #{built}|]

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

-- '/' should get URL-encoded to %2F, but we saw Nix not doing this??
--
-- Here's what I found in the Nix code:
-- const static std::string allowedInQuery = ":@/?";
-- const static std::string allowedInPath = ":@/";
--
-- The SRI hash can contain +, /, and = apparently (plus numbers and upper/lowercase letters)
-- So the only thing we need to allow here is '/'.
customIsAllowed :: Char -> Bool
customIsAllowed '/' = True
customIsAllowed x = URI.isAllowed x
