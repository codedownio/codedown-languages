{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

-- | Helpers and types for testing kernels' variable inspectors.
--
-- A kernel advertises its inspector via @metadata.codedown.variable_inspector@ in
-- its @kernel.json@: an @initial_code_path@ (run once at startup) plus
-- @list_variables_command@ and @inspect_variable_command@. The list command prints
-- a JSON object (name -> 'VariableInfo'); the inspect command prints a single
-- 'VariableDetail'. These helpers read that config from the built kernel, run the
-- commands against a real kernel, and decode the output.

module TestLib.VariableInspector (
  -- * Types
  VariableInfo(..)
  , VariableTable(..)
  , VariableDetail(..)
  , InspectorConfig(..)

  -- * Helpers
  , getInspectorConfig
  , withListVariables
  , withInspectVariable
  ) where

import Control.Monad.IO.Class
import Data.Aeson as A
import Data.Aeson.TH as A
import Data.Map (Map)
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import System.FilePath
import Test.Sandwich
import TestLib.Aeson
import TestLib.JupyterRunnerContext
import TestLib.JupyterTypes
import TestLib.Types
import TestLib.Util
import UnliftIO.Directory


-- | One entry in the variable listing. JSON keys are camelCase (@type@, @size@,
-- @shape@, @content@, @isMatrix@, @isWidget@). @isWidget@ is Python-only and
-- optional. Note: @shape@ elements are ints in practice; the pyspark case can emit
-- a leading null, which these tests don't exercise.
data VariableInfo = VariableInfo {
  variableInfoType :: Text
  , variableInfoSize :: Maybe Int
  , variableInfoShape :: Maybe [Int]
  , variableInfoContent :: Text
  , variableInfoIsMatrix :: Bool
  , variableInfoIsWidget :: Maybe Bool
  } deriving (Show, Eq)
$(deriveJSON (dropNAndToCamelCaseOptions (length ("variableInfo" :: String))) ''VariableInfo)

-- | Tabular payload for the detail view.
data VariableTable = VariableTable {
  variableTableColumns :: [Text]
  , variableTableData :: [[A.Value]]
  } deriving (Show, Eq)
$(deriveJSON (dropNAndToCamelCaseOptions (length ("variableTable" :: String))) ''VariableTable)

-- | The detailed single-variable view returned by the inspect command.
data VariableDetail = VariableDetail {
  variableDetailName :: Text
  , variableDetailType :: Maybe Text
  , variableDetailSize :: Maybe Int
  , variableDetailShape :: Maybe [Int]
  , variableDetailIsMatrix :: Bool
  , variableDetailContent :: Text
  , variableDetailTable :: Maybe VariableTable
  } deriving (Show, Eq)
$(deriveJSON (dropNAndToCamelCaseOptions (length ("variableDetail" :: String))) ''VariableDetail)

-- | The variable-inspector contract, read from kernel.json (with the initial code
-- already loaded from its file). @inspectorInspectCommand@ is optional (e.g. Rust
-- has no inspect command).
data InspectorConfig = InspectorConfig {
  inspectorInitialCode :: Text
  , inspectorListCommand :: Text
  , inspectorInspectCommand :: Maybe Text
  } deriving (Show, Eq)


-- | Read @metadata.codedown.variable_inspector@ from the built kernel's kernel.json
-- and load the initial-code file. Fails the test if any field is missing (which
-- also exercises the Nix wiring).
getInspectorConfig :: (
  HasNixEnvironment context, JupyterRunnerMonad m
  ) => Text -> ExampleT context m InspectorConfig
getInspectorConfig kernel = do
  nixEnv <- getContext nixEnvironment
  let kernelsDir = nixEnv </> "lib" </> "codedown" </> "kernels"

  -- Jupyter normalizes kernel names to lowercase for the spec directory, so match
  -- the directory case-insensitively (e.g. test kernel "R" -> directory "r").
  entries <- liftIO $ listDirectory kernelsDir
  kernelDir <- case filter ((== T.toLower kernel) . T.toLower . T.pack) entries of
    (d:_) -> return d
    [] -> expectationFailure [i|No kernel directory matching '#{kernel}' in #{kernelsDir} (found: #{entries})|]
  let kernelJson = kernelsDir </> kernelDir </> "kernel.json"

  value <- liftIO (A.eitherDecodeFileStrict kernelJson) >>= \case
    Left err -> expectationFailure [i|Failed to decode '#{kernelJson}': #{err}|]
    Right v -> return v

  viObj <- case lookupKey "metadata" value >>= lookupKey "codedown" >>= lookupKey "variable_inspector" of
    Just (A.Object o) -> return (A.Object o)
    _ -> expectationFailure [i|kernel.json for '#{kernel}' has no metadata.codedown.variable_inspector|]

  initialCodePath <- requireString viObj "initial_code_path"
  listCommand <- requireString viObj "list_variables_command"

  let inspectCommand = case lookupKey "inspect_variable_command" viObj of
        Just (A.String s) -> Just s
        _ -> Nothing

  initialCode <- liftIO $ TIO.readFile (T.unpack initialCodePath)

  return $ InspectorConfig initialCode listCommand inspectCommand

  where
    requireString obj key = case lookupKey key obj of
      Just (A.String s) -> return s
      _ -> expectationFailure [i|variable_inspector for '#{kernel}' is missing string field '#{key}'|]

-- | Run @setup@, then the inspector's initial code, then its list command, and
-- decode the printed JSON into a map of variable name -> 'VariableInfo'.
withListVariables :: (
  HasJupyterRunnerContext context, JupyterRunnerMonad m
  ) => Text -> Text -> (Map Text VariableInfo -> ExampleT context m ()) -> ExampleT context m ()
withListVariables kernel setup cb = do
  InspectorConfig {..} <- getInspectorConfig kernel
  -- Run the initial code first (as the frontend does at startup), then the user
  -- setup, then the command, and decode the printed JSON from stdout. (Some
  -- inspectors, e.g. bash, snapshot a baseline of variables at init time.)
  --
  -- The frontend always runs the list command as its own execute request; most
  -- kernels also tolerate it appended to setup in one cell, but evcxr's
  -- meta-commands (":...") must be their own cell, so split in that case.
  let code = if ":" `T.isPrefixOf` T.strip inspectorListCommand
             then T.intercalate "\n" [inspectorInitialCode, setup] <> "\n@@@CELL@@@\n" <> inspectorListCommand
             else T.intercalate "\n" [inspectorInitialCode, setup, inspectorListCommand]
  runInspectorCommand kernel code "variable list" cb

-- | Like 'withListVariables', but runs the inspect command for a single variable
-- (substituting its name into the @{{VARIABLE_NAME}}@ placeholder) and decodes a
-- 'VariableDetail'.
withInspectVariable :: (
  HasJupyterRunnerContext context, JupyterRunnerMonad m
  ) => Text -> Text -> Text -> (VariableDetail -> ExampleT context m ()) -> ExampleT context m ()
withInspectVariable kernel setup varName cb = do
  InspectorConfig {..} <- getInspectorConfig kernel
  case inspectorInspectCommand of
    Nothing -> expectationFailure [i|kernel '#{kernel}' has no inspect_variable_command|]
    Just tmpl -> do
      let inspectCommand = T.replace "{{VARIABLE_NAME}}" varName tmpl
      let code = T.intercalate "\n" [inspectorInitialCode, setup, inspectCommand]
      runInspectorCommand kernel code [i|inspect of '#{varName}'|] cb

-- | Run the code and decode the command's JSON output. Most kernels print JSON to
-- stdout; evcxr's :vars_json command returns it as an execute_result instead, so we
-- fall back to that. Decodes the last non-empty line as JSON.
runInspectorCommand :: (
  HasJupyterRunnerContext context, JupyterRunnerMonad m, FromJSON a
  ) => Text -> Text -> Text -> (a -> ExampleT context m ()) -> ExampleT context m ()
runInspectorCommand kernel code what cb =
  runKernelCode kernel code $ \notebookFile outputNotebook outFile _errFile -> do
    stdoutContents <- doesFileExist outFile >>= \case
      True -> liftIO (TIO.readFile outFile)
      False -> return ""
    candidate <- case lastNonEmptyLine stdoutContents of
      Just l -> return (Just l)
      Nothing -> liftIO (A.eitherDecodeFileStrict outputNotebook) >>= \case
        Right nb -> return (lastNonEmptyLine =<< lastExecuteResultText nb)
        Left err -> expectationFailure [i|Failed to decode notebook '#{notebookFile}': #{err}|]
    case candidate of
      Nothing -> expectationFailure [i|Variable inspector (#{what}) produced no output|]
      Just jsonLine -> case A.eitherDecodeStrict (TE.encodeUtf8 jsonLine) of
        Left err -> expectationFailure [i|Failed to parse #{what} JSON: #{err}\nGot: #{jsonLine}|]
        Right x -> cb x

-- | The text/plain of the last execute_result in the notebook (evcxr's :vars_json
-- output lands here rather than on stdout).
lastExecuteResultText :: JupyterNotebook -> Maybe Text
lastExecuteResultText (JupyterNotebook {..}) =
  case [ mimeText executeResultData | CodeCell {..} <- notebookCells
                                    , ExecuteResultOutput {..} <- codeOutputs ] of
    [] -> Nothing
    xs -> Just (last xs)
  where
    mimeText m = case M.lookup (MimeType "text/plain") m of
      Just (A.String s) -> s
      Just (A.Array xs) -> T.concat [s | A.String s <- V.toList xs]
      _ -> ""

lookupKey :: Text -> A.Value -> Maybe A.Value
lookupKey k (A.Object o) = aesonLookup k o
lookupKey _ _ = Nothing

lastNonEmptyLine :: Text -> Maybe Text
lastNonEmptyLine t = case filter (not . T.null . T.strip) (T.lines t) of
  [] -> Nothing
  xs -> Just (last xs)
