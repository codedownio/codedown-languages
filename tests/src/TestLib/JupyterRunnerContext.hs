{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module TestLib.JupyterRunnerContext where

import Control.Monad
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Aeson as A
import Data.ByteString.Lazy.Char8 as BL
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text as T
import Data.Text.IO as T
import qualified Data.Vector as V
import System.Exit
import System.FilePath
import Test.Sandwich
import TestLib.JupyterTypes
import TestLib.Types
import TestLib.Util
import UnliftIO.Directory
import UnliftIO.Process


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

parseJson :: V.Vector A.Value -> Maybe Text
parseJson ((flip (V.!?) 0) -> Just (A.Object (aesonLookup "outputs" -> Just (A.Object (aesonLookup "out" -> Just (A.String t)))))) = Just t
parseJson _ = Nothing

type HasJupyterRunnerContext context = (
  HasJupyterRunner context
  , HasNixEnvironment context
  , HasBaseContext context
  )

type JupyterRunnerMonad m = (
  MonadBaseControl IO m
  , MonadUnliftIO m
  , MonadThrow m
  , MonadFail m
  )

testKernelStdout :: (HasJupyterRunnerContext context, JupyterRunnerMonad m) => Text -> Text -> Text -> SpecFree context m ()
testKernelStdout kernel code desired = it [i|#{kernel}: #{code} -> #{desired}|] $ testKernelStdout' kernel code (`shouldBe` desired)

testKernelStdoutCallback :: (HasJupyterRunnerContext context, JupyterRunnerMonad m) => Text -> Text -> (Text -> ExampleT context m ()) -> SpecFree context m ()
testKernelStdoutCallback kernel code cb = it [i|#{kernel}: #{code}|] $ testKernelStdout' kernel code cb

testKernelStdout' :: (HasJupyterRunnerContext context, JupyterRunnerMonad m) => Text -> Text -> (Text -> ExampleT context m ()) -> ExampleT context m ()
testKernelStdout' kernel code cb = do
  runKernelCode kernel code $ \_notebookFile _outputNotebookFile outFile _errFile -> do
    doesFileExist outFile >>= \case
      True -> liftIO (T.readFile outFile) >>= cb
      False -> cb ""

itHasDisplayDatas :: (HasJupyterRunnerContext context, JupyterRunnerMonad m) => Text -> Text -> [Map MimeType A.Value] -> SpecFree context m ()
itHasDisplayDatas kernel code desired = it [i|#{kernel}: #{show code} -> #{desired}|] $ displayDatasShouldBe kernel code desired

itHasDisplayTexts :: (HasJupyterRunnerContext context, JupyterRunnerMonad m) => Text -> Text -> [Maybe A.Value] -> SpecFree context m ()
itHasDisplayTexts kernel code desired = it [i|#{kernel}: #{show code} -> #{desired}|] $ displayTextsShouldBe kernel code desired

displayDatasShouldBe :: (HasJupyterRunnerContext context, JupyterRunnerMonad m) => Text -> Text -> [Map MimeType A.Value] -> ExampleT context m ()
displayDatasShouldBe kernel code desired = displayDatasShouldSatisfy kernel code (`shouldBe` desired)

displayTextsShouldBe :: (HasJupyterRunnerContext context, JupyterRunnerMonad m) => Text -> Text -> [Maybe A.Value] -> ExampleT context m ()
displayTextsShouldBe kernel code desired = displayDatasShouldSatisfy kernel code ((`shouldBe` desired) . fmap (M.lookup (MimeType "text/plain")))

displayDatasShouldSatisfy :: (
  HasJupyterRunnerContext context, JupyterRunnerMonad m
  ) => Text -> Text -> ([Map MimeType A.Value] -> ExampleT context m ()) -> ExampleT context m ()
displayDatasShouldSatisfy kernel code cb = notebookShouldSatisfy kernel code $ \(JupyterNotebook {..}) -> do
  let outputs = mconcat [codeOutputs | CodeCell {..} <- notebookCells]
  cb ([displayDataData | DisplayDataOutput {..} <- outputs])

-- * Execute result helpers

itHasExecuteDatas :: (HasJupyterRunnerContext context, JupyterRunnerMonad m) => Text -> Text -> [Map MimeType A.Value] -> SpecFree context m ()
itHasExecuteDatas kernel code desired = it [i|#{kernel}: #{show code} -> #{desired}|] $ executeDatasShouldBe kernel code desired

itHasExecuteTexts :: (HasJupyterRunnerContext context, JupyterRunnerMonad m) => Text -> Text -> [Maybe A.Value] -> SpecFree context m ()
itHasExecuteTexts kernel code desired = it [i|#{kernel}: #{show code} -> #{desired}|] $ executeTextsShouldBe kernel code desired

executeDatasShouldBe :: (HasJupyterRunnerContext context, JupyterRunnerMonad m) => Text -> Text -> [Map MimeType A.Value] -> ExampleT context m ()
executeDatasShouldBe kernel code desired = executeResultsShouldSatisfy kernel code (`shouldBe` desired)

executeTextsShouldBe :: (HasJupyterRunnerContext context, JupyterRunnerMonad m) => Text -> Text -> [Maybe A.Value] -> ExampleT context m ()
executeTextsShouldBe kernel code desired = executeResultsShouldSatisfy kernel code ((`shouldBe` desired) . fmap (M.lookup (MimeType "text/plain")))

executeResultsShouldSatisfy :: (
  HasJupyterRunnerContext context, JupyterRunnerMonad m
  ) => Text -> Text -> ([Map MimeType A.Value] -> ExampleT context m ()) -> ExampleT context m ()
executeResultsShouldSatisfy kernel code cb = notebookShouldSatisfy kernel code $ \(JupyterNotebook {..}) -> do
  let outputs = mconcat [codeOutputs | CodeCell {..} <- notebookCells]
  cb ([executeResultData | ExecuteResultOutput {..} <- outputs])

-- * Core notebook helper

notebookShouldSatisfy :: (
  HasJupyterRunnerContext context, JupyterRunnerMonad m
  ) => Text -> Text -> (JupyterNotebook -> ExampleT context m ()) -> ExampleT context m ()
notebookShouldSatisfy kernel code cb = do
  runKernelCode kernel code $ \notebookFile outputNotebookFile _outFile _errFile -> do
    liftIO (A.eitherDecodeFileStrict outputNotebookFile) >>= \case
      Left err -> expectationFailure [i|Failed to decode notebook '#{notebookFile}': #{err}|]
      Right nb -> cb nb

runKernelCode :: (
  (HasJupyterRunnerContext context, JupyterRunnerMonad m)
  , MonadReader context m
  , MonadLoggerIO m
  ) => Text -> Text -> (FilePath -> FilePath -> FilePath -> FilePath -> m b) -> m b
runKernelCode kernel code cb = do
  nixEnv <- getContext nixEnvironment
  let jupyterPath = nixEnv </> "lib" </> "codedown"
  debug [i|Got jupyterPath: #{jupyterPath}|]

  jr <- getContext jupyterRunner
  debug [i|Got jupyterRunner: #{jr}|]

  Just folder <- getCurrentFolder

  let runDir = folder </> "run"
  createDirectoryIfMissing True runDir

  let notebook = runDir </> "notebook.ipynb"
  let outFile = runDir </> "out.txt"
  let errFile = runDir </> "err.txt"

  let homeDir = folder </> "home"
  createDirectoryIfMissing True homeDir

  liftIO $ BL.writeFile notebook (A.encode (notebookWithCode kernel code))

  let cp = (proc jr ["notebook.ipynb", "out.ipynb"
                    , "--stdout-file", outFile
                    , "--stderr-file", errFile
                    , "--start-timeout", "120"
                    , "-k", T.unpack kernel
                    ]) {
        env = Just [
            ("JUPYTER_PATH", jupyterPath)
            , ("HOME", homeDir)
            ]
        , cwd = Just runDir
        }
  createProcessWithLogging cp >>= waitForProcess >>= (`shouldBe` ExitSuccess)

  cb notebook (runDir </> "out.ipynb") outFile errFile


notebookWithCode :: Text -> Text -> A.Value
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
                  , ("source", A.Array (V.fromList (fmap A.String ls)))
                  , ("outputs", A.Array [])
                  ]])
  ]

  where
    rawLines = T.splitOn "\n" code
    ls = [x <> "\n" | x <- L.init rawLines] <> [L.last rawLines]
