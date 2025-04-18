{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
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
import Data.ByteString.Lazy.Char8 as BL8
import Data.Function
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

introduceBootstrapNixpkgs :: (
  HasBaseContext context, MonadIO m, MonadMask m, MonadUnliftIO m, MonadBaseControl IO m
  ) => SpecFree (LabelValue "bootstrapNixpkgs" FilePath :> context) m () -> SpecFree context m ()
introduceBootstrapNixpkgs = introduceWith [i|Jupyter runner|] bootstrapNixpkgs $ \action -> do
  rootDir <- findFirstParentMatching (\x -> doesPathExist (x </> ".git"))

  out <- readCreateProcessWithLogging ((proc "nix" ["run", ".#nixpkgsPath"]) { cwd = Just (rootDir </> "tests") }) ""
  void $ action (T.unpack $ T.strip $ T.pack out)

-- | TODO: pipe through a command-line argument to control whether bwrap is used?
introduceJustBubblewrap :: (
  HasBaseContext context, MonadIO m, MonadMask m, MonadUnliftIO m, MonadBaseControl IO m
  ) => SpecFree (LabelValue "maybeBubblewrap" (Maybe FilePath) :> context) m () -> SpecFree context m ()
introduceJustBubblewrap = introduceWith [i|bwrap|] maybeBubblewrap $ \action -> do
#ifdef darwin_HOST_OS
  void $ action Nothing
#else
  liftIO (findExecutable "bwrap") >>= \case
    Nothing -> expectationFailure [i|The tests currently require bubblewrap to be present.|]
    Just path -> void $ action (Just path)
#endif

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
testKernelStdout kernel code desired = it [i|#{kernel} -- #{summarizeCode code} -> #{summarizeCode desired}|] $
  testKernelStdout'' kernel code (`shouldBe` Just desired)

testKernelStdout' :: (
  HasJupyterRunnerContext context, JupyterRunnerMonad m
  ) => Text -> Text -> Maybe Text -> SpecFree context m ()
testKernelStdout' kernel code desired = it [i|#{kernel} -- #{summarizeCode code} -> #{summarizeCode <$> desired}|] $
  testKernelStdout'' kernel code (`shouldBe` desired)

testKernelStdoutCallback :: (
  HasJupyterRunnerContext context, JupyterRunnerMonad m
  ) => Text -> Text -> (Maybe Text -> ExampleT context m ()) -> SpecFree context m ()
testKernelStdoutCallback kernel code cb = it [i|#{kernel} -- #{summarizeCode code}|] $
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
itHasDisplayDatas kernel code desired = it [i|#{kernel} -- #{summarizeCode code} -> #{desired}|] $
  displayDatasShouldBe kernel code desired

itHasDisplayTexts :: (
  HasJupyterRunnerContext context, JupyterRunnerMonad m
  ) => Text -> Text -> [Maybe A.Value] -> SpecFree context m ()
itHasDisplayTexts kernel code desired = it [i|#{kernel} -- #{summarizeCode code} -> #{desired}|] $
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
itHasExecuteDatas kernel code desired = it [i|#{kernel} -- #{summarizeCode code} -> #{desired}|] $
  executeDatasShouldBe kernel code desired

itHasExecuteTexts :: (
  HasJupyterRunnerContext context, JupyterRunnerMonad m
  ) => Text -> Text -> [Maybe A.Value] -> SpecFree context m ()
itHasExecuteTexts kernel code desired = it [i|#{kernel} -- #{summarizeCode code} -> #{desired}|] $
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

summarizeCode :: Text -> Text
summarizeCode code = code
  & truncateCode
  & replaceBadCharacters
  where
    maxLen = 15

    truncateCode :: Text -> Text
    truncateCode t
      | T.length t <= maxLen = t
      | otherwise = T.take maxLen t <> "..."

    -- Colons will cause an error like this on macOS:
    -- error: failed to join paths from `$DYLD_FALLBACK_LIBRARY_PATH` together
    -- path segment contains separator `:`
    replaceBadCharacters t = t &
      T.replace ":" "_"

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

  liftIO $ BL8.writeFile (relativeToOuterRunDir notebook) (A.encode (notebookWithCode kernel code))

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
      -- Provide some core utils on the PATH. For example, the R kernel needs this:
      -- https://github.com/codedownio/codedown-languages/issues/63
      binDirJson <- readCreateProcessWithLogging (
        proc "nix" ["build"
                   , "--impure"
                   , "--expr", [i|with import <nixpkgs> {}; pkgsStatic.busybox|]
                   , "--json"
                   , "--no-link"
                   ]) ""
      binDir <- case parseNixBuildJson <$> (A.eitherDecode (BL8.pack binDirJson)) of
        Left err -> expectationFailure [i|Failed to parse bin paths: #{err}. JSON: #{binDirJson}|]
        Right Nothing -> expectationFailure [i|Didn't find Nix output in: #{binDirJson}|]
        Right (Just output) -> pure ((T.unpack output) </> "bin")
      info [i|PATH: #{binDir </> "bin"}|]

      -- Get the full closure of the Nix environment and jupyter runner
      fullClosure <- (Prelude.filter (/= "") . T.splitOn "\n" . T.pack) <$> readCreateProcessWithLogging (
        proc "nix" (["path-info", "-r"
                    , jr
                    , nixEnv
                    , binDir
                    ])
        ) ""

      let bwrapArgs = ["--tmpfs", "/tmp"
                      , "--bind", outerRunDir, innerRunDir
                      , "--bind", outerHomeDir, "/home"
                      , "--clearenv"
                      , "--setenv", "HOME", "/home"
                      , "--setenv", "JUPYTER_PATH", jupyterPath
                      , "--chdir", innerRunDir

                      , "--ro-bind", binDir, "/bin"
                      , "--setenv", "PATH", "/bin"

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
  introduceJustBubblewrap $
  introduceBootstrapNixpkgs
  tests
