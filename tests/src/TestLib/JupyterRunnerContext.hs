{-# LANGUAGE OverloadedLists #-}

module TestLib.JupyterRunnerContext where

import Control.Monad
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Aeson as A
import Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as HM
import Data.String.Interpolate
import Data.Text as T
import Data.Text.IO as T
import qualified Data.Vector as V
import System.Directory (getSymbolicLinkTarget)
import System.Exit
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Logging
import TestLib.Aeson
import TestLib.NixRendering
import TestLib.NixTypes
import TestLib.Types
import TestLib.Util
import UnliftIO.Directory
import UnliftIO.Process
import UnliftIO.Temporary


introduceJupyterRunner :: (
  HasBaseContext context, MonadIO m, MonadMask m, MonadUnliftIO m, MonadBaseControl IO m
  ) => SpecFree (LabelValue "jupyterRunner" FilePath :> context) m () -> SpecFree context m ()
introduceJupyterRunner = introduceWith [i|Jupyter runner|] jupyterRunner $ \action -> do
  rootDir <- findFirstParentMatching (\x -> doesPathExist (x </> ".git"))

  Just currentDir <- getCurrentFolder
  let runnerPath = currentDir </> "jupyter-runner"

  let cp = ((proc "nix" ["build", ".#jupyter-runner", "-o", runnerPath]) { cwd = Just rootDir })
  createProcessWithLogging cp >>= waitForProcess >>= (`shouldBe` ExitSuccess)
  void $ action runnerPath

parseJson ((flip (V.!?) 0) -> Just (A.Object (HM.lookup "outputs" -> Just (A.Object (HM.lookup "out" -> Just (A.String t)))))) = Just t
parseJson _ = Nothing

testKernelStdout :: (
  HasJupyterRunner context
  , HasNixEnvironment context
  , HasBaseContext context
  , MonadIO m
  , MonadBaseControl IO m
  , MonadUnliftIO m
  , MonadThrow m
  ) => Text -> Text -> Text -> SpecFree context m ()
testKernelStdout kernel code desired = it [i|#{kernel}: #{code} -> #{desired}|] $ do
  nixEnv <- getContext nixEnvironment
  let jupyterPath = nixEnv </> "lib" </> "codedown"
  debug [i|Got jupyterPath: #{jupyterPath}|]

  jr <- getContext jupyterRunner
  debug [i|Got jupyterRunner: #{jr}|]

  Just folder <- getCurrentFolder
  let runDir = folder </> "run"
  let notebook = runDir </> "notebook.ipynb"
  let outFile = runDir </> "out.txt"
  let errFile = runDir </> "err.txt"
  createDirectoryIfMissing True runDir
  liftIO $ BL.writeFile notebook (A.encode (notebookWithCode kernel code))

  let cp = (proc jr ["notebook.ipynb", "out.ipynb"
                    , "--stdout-file", outFile
                    , "--stderr-file", errFile
                    , "-k", T.unpack kernel
                    ]) {
        env = Just [("JUPYTER_PATH", jupyterPath)]
        , cwd = Just runDir
        }
  createProcessWithLogging cp >>= waitForProcess >>= (`shouldBe` ExitSuccess)

  doesFileExist outFile >>= \case
    True -> liftIO (T.readFile outFile) >>= (`shouldBe` desired)
    False -> "" `shouldBe` desired

notebookWithCode kernel code = A.object [
  ("nbformat", A.Number 4)
  , ("nbformat_minor", A.Number 2)
  , ("metadata", A.object [
        ("kernel_info", A.object [
            ("name", A.String kernel)
            , ("display_name", A.String "Unknown")
            ]
        )

        , ("language_info", A.object [
            ("file_extension", A.String ".ipynb")
            , ("name", A.String kernel)
            , ("version", A.String "5.0")
            ]
        )

        ])
  , ("cells", A.Array [A.Object [
                  ("cell_type", A.String "code")
                  , ("metadata", A.object [])
                  , ("execution_count", A.Null)
                  , ("source", A.Array (V.fromList [A.String x | x <- (T.splitOn "\n" code)]))
                  , ("outputs", A.Array [])
                  ]])
  ]
