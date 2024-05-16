{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}

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
import Test.Sandwich as Sandwich
import TestLib.JupyterTypes
import TestLib.Types
import TestLib.Util
import UnliftIO.Directory
import UnliftIO.Process

-- import qualified Data.ByteString.Lazy.Char8 as BL8


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

-- | TODO: pipe through a command-line argument to control whether bwrap is used?
introduceJustBubblewrap :: (
  HasBaseContext context, MonadIO m, MonadMask m, MonadUnliftIO m, MonadBaseControl IO m
  ) => SpecFree (LabelValue "maybeBubblewrap" (Maybe FilePath) :> context) m () -> SpecFree context m ()
introduceJustBubblewrap = introduceWith [i|bwrap|] maybeBubblewrap $ \action -> do
  liftIO (findExecutable "bwrap") >>= \case
    Nothing -> expectationFailure [i|The tests currently require bubblewrap to be present.|]
    Just path -> void $ action (Just path)

introduceNothingBubblewrap :: (
  HasBaseContext context, MonadIO m, MonadMask m, MonadUnliftIO m, MonadBaseControl IO m
  ) => SpecFree (LabelValue "maybeBubblewrap" (Maybe FilePath) :> context) m () -> SpecFree context m ()
introduceNothingBubblewrap = introduceWith [i|Nothing bubblewrap|] maybeBubblewrap $ \action ->
  void $ action Nothing

parseNixBuildJson :: V.Vector A.Value -> Maybe Text
parseNixBuildJson ((flip (V.!?) 0) -> Just (A.Object (aesonLookup "outputs" -> Just (A.Object (aesonLookup "out" -> Just (A.String t)))))) = Just t
parseNixBuildJson _ = Nothing

type HasJupyterRunnerContext context = (
  HasJupyterRunner context
  , HasMaybeBubblewrap context
  , HasNixEnvironment context
  , HasBaseContext context
  )

type JupyterRunnerMonad m = (
  MonadBaseControl IO m
  , MonadUnliftIO m
  , MonadThrow m
  , MonadFail m
  )

testKernelStdout :: (
  HasJupyterRunnerContext context, JupyterRunnerMonad m
  ) => Text -> Text -> Text -> SpecFree context m ()
testKernelStdout kernel code desired = it [i|#{kernel}: #{code} -> #{desired}|] $
  testKernelStdout'' kernel code (`shouldBe` Just desired)

testKernelStdout' :: (
  HasJupyterRunnerContext context, JupyterRunnerMonad m
  ) => Text -> Text -> Maybe Text -> SpecFree context m ()
testKernelStdout' kernel code desired = it [i|#{kernel}: #{code} -> #{desired}|] $
  testKernelStdout'' kernel code (`shouldBe` desired)

testKernelStdoutCallback :: (
  HasJupyterRunnerContext context, JupyterRunnerMonad m
  ) => Text -> Text -> (Maybe Text -> ExampleT context m ()) -> SpecFree context m ()
testKernelStdoutCallback kernel code cb = it [i|#{kernel}: #{code}|] $
  testKernelStdout'' kernel code cb

testKernelStdout'' :: (
  HasJupyterRunnerContext context, JupyterRunnerMonad m
  ) => Text -> Text -> (Maybe Text -> ExampleT context m ()) -> ExampleT context m ()
testKernelStdout'' kernel code cb = do
  runKernelCode kernel code $ \_notebookFile _outputNotebookFile outFile _errFile -> do
    doesFileExist outFile >>= \case
      True -> liftIO (T.readFile outFile) >>= cb . Just
      False -> cb Nothing

itHasDisplayDatas :: (
  HasJupyterRunnerContext context, JupyterRunnerMonad m
  ) => Text -> Text -> [Map MimeType A.Value] -> SpecFree context m ()
itHasDisplayDatas kernel code desired = it [i|#{kernel}: #{show code} -> #{desired}|] $
  displayDatasShouldBe kernel code desired

itHasDisplayTexts :: (
  HasJupyterRunnerContext context, JupyterRunnerMonad m
  ) => Text -> Text -> [Maybe A.Value] -> SpecFree context m ()
itHasDisplayTexts kernel code desired = it [i|#{kernel}: #{show code} -> #{desired}|] $
  displayTextsShouldBe kernel code desired

displayDatasShouldBe :: (
  HasJupyterRunnerContext context, JupyterRunnerMonad m
  ) => Text -> Text -> [Map MimeType A.Value] -> ExampleT context m ()
displayDatasShouldBe kernel code desired = displayDatasShouldSatisfy kernel code (`shouldBe` desired)

displayTextsShouldBe :: (
  HasJupyterRunnerContext context, JupyterRunnerMonad m
  ) => Text -> Text -> [Maybe A.Value] -> ExampleT context m ()
displayTextsShouldBe kernel code desired = displayDatasShouldSatisfy kernel code ((`shouldBe` desired) . fmap (M.lookup (MimeType "text/plain")))

displayDatasShouldSatisfy :: (
  HasJupyterRunnerContext context, JupyterRunnerMonad m
  ) => Text -> Text -> ([Map MimeType A.Value] -> ExampleT context m ()) -> ExampleT context m ()
displayDatasShouldSatisfy kernel code cb = notebookShouldSatisfy kernel code $ \(JupyterNotebook {..}) -> do
  let outputs = mconcat [codeOutputs | CodeCell {..} <- notebookCells]
  cb ([displayDataData | DisplayDataOutput {..} <- outputs])

-- * Execute result helpers

itHasExecuteDatas :: (
  HasJupyterRunnerContext context, JupyterRunnerMonad m
  ) => Text -> Text -> [Map MimeType A.Value] -> SpecFree context m ()
itHasExecuteDatas kernel code desired = it [i|#{kernel}: #{show code} -> #{desired}|] $
  executeDatasShouldBe kernel code desired

itHasExecuteTexts :: (
  HasJupyterRunnerContext context, JupyterRunnerMonad m
  ) => Text -> Text -> [Maybe A.Value] -> SpecFree context m ()
itHasExecuteTexts kernel code desired = it [i|#{kernel}: #{show code} -> #{desired}|] $
  executeTextsShouldBe kernel code desired

executeDatasShouldBe :: (
  HasJupyterRunnerContext context, JupyterRunnerMonad m
  ) => Text -> Text -> [Map MimeType A.Value] -> ExampleT context m ()
executeDatasShouldBe kernel code desired = executeResultsShouldSatisfy kernel code (`shouldBe` desired)

executeTextsShouldBe :: (
  HasJupyterRunnerContext context, JupyterRunnerMonad m
  ) => Text -> Text -> [Maybe A.Value] -> ExampleT context m ()
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
  HasJupyterRunnerContext context
  , JupyterRunnerMonad m
  , MonadReader context m
  , MonadLoggerIO m
  ) => Text -> Text -> (FilePath -> FilePath -> FilePath -> FilePath -> m b) -> m b
runKernelCode kernel code cb = do
  nixEnv <- getContext nixEnvironment
  let jupyterPath = nixEnv </> "lib" </> "codedown"
  debug [i|Got jupyterPath: #{jupyterPath}|]

  -- Get the path to the Jupyter runner in the Nix store
  jr <- getContext jupyterRunner >>= canonicalizePath
  debug [i|Got jupyterRunner: #{jr}|]

  maybeBwrap <- getContext maybeBubblewrap

  let notebook = "notebook.ipynb"
  let outFile = "out.txt"
  let errFile = "err.txt"

  Just folder <- getCurrentFolder

  let outerHomeDir = folder </> "home"
  createDirectoryIfMissing True outerHomeDir

  let outerRunDir = folder </> "run"
  createDirectoryIfMissing True outerRunDir
  let relativeToOuterRunDir = (outerRunDir </>)

  let innerRunDir = case maybeBwrap of
        Just _ -> "/papermill_run"
        Nothing -> outerRunDir
  let relativeToInnerRunDir = case maybeBwrap of
        Just _ -> (innerRunDir </>)
        Nothing -> (outerRunDir </>)

  liftIO $ BL.writeFile (relativeToOuterRunDir notebook) (A.encode (notebookWithCode kernel code))

  let papermillArgs = [
        "notebook.ipynb", "out.ipynb"
        , "--stdout-file", relativeToInnerRunDir outFile
        , "--stderr-file", relativeToInnerRunDir errFile
        , "--start-timeout", "120"
        , "--cwd", innerRunDir
        , "--log-level", "DEBUG"
        , "-k", T.unpack kernel
        ]

  cp <- case maybeBwrap of
    Just bwrap -> do
      -- https://github.com/codedownio/codedown-languages/issues/63
      -- binPathsJson <- readCreateProcessWithLogging (
      --   proc "nix" ["build"
      --              , "--impure"
      --              , "--expr", [i|with import <nixpkgs> {}; symlinkJoin { name = "base-paths"; paths = [which coreutils]; }|]
      --              , "--json"
      --              ]) ""
      -- binPaths <- case parseNixBuildJson <$> (A.eitherDecode (BL8.pack binPathsJson)) of
      --   Left err -> expectationFailure [i|Failed to parse bin paths: #{err}. JSON: #{binPathsJson}|]
      --   Right Nothing -> expectationFailure [i|Didn't find Nix output in: #{binPathsJson}|]
      --   Right (Just output) -> pure $ T.unpack output
      -- info [i|PATH: #{binPaths </> "bin"}|]

      -- Get the full closure of the Nix environment and jupyter runner
      fullClosure <- (Prelude.filter (/= "") . T.splitOn "\n" . T.pack) <$> readCreateProcessWithLogging (
        proc "nix" (["path-info", "-r"
                    , jr
                    , nixEnv
                    -- , binPaths
                    ])
        ) ""

      let bwrapArgs = ["--tmpfs", "/tmp"
                      , "--bind", outerRunDir, innerRunDir
                      , "--bind", outerHomeDir, "/home"
                      , "--clearenv"
                      , "--setenv", "HOME", "/home"
                      , "--setenv", "JUPYTER_PATH", jupyterPath
                      , "--chdir", innerRunDir

                      -- , "--setenv", "PATH", binPaths </> "bin"

                      , "--proc", "/proc"
                      , "--dev", "/dev"
                      ]
                      <> mconcat [["--ro-bind", x, x] | x <- fmap T.unpack fullClosure]
                      <> ["--"]
                      <> (jr : papermillArgs)

      info [i|#{bwrap} #{T.intercalate " " $ fmap T.pack bwrapArgs}|]
      return $ proc bwrap bwrapArgs
    Nothing -> do
      info [i|#{jr} #{T.intercalate " " $ fmap T.pack papermillArgs}|]
      return $ (proc jr papermillArgs) {
        env = Just [
            ("JUPYTER_PATH", jupyterPath)
            , ("HOME", outerHomeDir)
            ]
        , cwd = Just outerRunDir
        }

  createProcessWithLogging cp >>= waitForProcess >>= (`shouldBe` ExitSuccess)

  cb notebook (relativeToOuterRunDir "out.ipynb") (relativeToOuterRunDir outFile) (relativeToOuterRunDir errFile)


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

-- * Utility main function

jupyterMain :: LanguageSpec -> IO ()
jupyterMain tests = runSandwichWithCommandLineArgs' Sandwich.defaultOptions specialOptions $
  introduceJupyterRunner $
  introduceJustBubblewrap
  tests
