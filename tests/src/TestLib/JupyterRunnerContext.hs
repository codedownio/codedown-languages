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

  let cp = ((proc "nix" ["build", ".#jupyter-runner", "--json"]) { cwd = Just rootDir })
  jupyterRunnerRaw :: A.Array <- ((A.eitherDecode . BL.pack) <$>) (readCreateProcess cp "") >>= \case
    Left err -> expectationFailure [i|Failed to parse build JSON: #{err}|]
    Right x -> return x

  case parseJson jupyterRunnerRaw of
    Nothing -> expectationFailure [i|Couldn't parse build path|]
    Just path -> void $ action $ T.unpack path

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
  createDirectoryIfMissing True runDir
  liftIO $ BL.writeFile notebook (A.encode (notebookWithCode kernel code))

  let cp = (proc jr ["notebook.ipynb", "out.ipynb"
                    , "--stdout-file", runDir </> "out.txt"
                    , "--stderr-file", runDir </> "err.txt"
                    , "-k", T.unpack kernel
                    ]) {
        env = Just [("JUPYTER_PATH", jupyterPath)]
        , cwd = Just runDir
        }
  createProcessWithLogging cp >>= waitForProcess >>= (`shouldBe` ExitSuccess)

  liftIO (T.readFile (runDir </> "out.txt")) >>= (`shouldBe` desired)

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
