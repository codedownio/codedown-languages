{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module TestLib.Exporters where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson as A
import Data.Aeson.QQ.Simple
import Data.Aeson.TH as A
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Text.IO as T
import GHC.TypeLits (symbolVal)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Test
import Language.LSP.Test.Helpers (LspContext, LspSessionInfo(..), LspSessionOptions(..))
import qualified Language.LSP.Test.Helpers as Helpers
import Safe
import System.FilePath
import Test.Sandwich as Sandwich
import TestLib.Aeson
import TestLib.LSP.Session (lspSessionOptionsFor)
import TestLib.Types
import UnliftIO.Directory
import UnliftIO.Process


data ExporterInfo = ExporterInfo {
  exporterInfoName :: Text
  , exporterInfoDisplayName :: Text
  , exporterInfoIcon :: Maybe Text
  , exporterInfoIconMonochrome :: Maybe Text
  , exporterInfoArgs :: [Text]
  , exporterInfoExtension :: Text
} deriving (Show, Eq)
deriveJSON toSnake2 ''ExporterInfo

sampleJupyterNotebook :: Value
sampleJupyterNotebook = [aesonQQ|{
  "cells": [
    {
      "cell_type": "code",
      "execution_count": null,
      "metadata": {},
      "outputs": [],
      "source": ["print('Hello from exporter test')"]
    }
  ],
  "metadata": {
    "kernelspec": {
      "display_name": "Python 3",
      "language": "python",
      "name": "python3"
    }
  },
  "nbformat": 4,
  "nbformat_minor": 4
}|]

sampleMdFile :: Text
sampleMdFile =
  [__i|\# Sample Markdown Document

       This is a sample markdown file to test exporter functionality.

       \#\# Features Demonstrated

       \#\#\# Text Formatting

       - **Bold text**
       - *Italic text*
       - `Inline code`
       - ~~Strikethrough text~~

       \#\#\# Code Blocks

       ```python
       def hello_world():
           print("Hello, World!")
           return 42
       ```

       \#\#\# Links and Images

       [Link to example](https://example.com)

       \#\#\# Tables

       | Column 1 | Column 2 | Column 3 |
       |----------|----------|----------|
       | Value A  | Value B  | Value C  |
       | Data 1   | Data 2   | Data 3   |

       \#\#\# Blockquotes

       > This is a blockquote.
       > It can span multiple lines.

       \#\#\# Math (if supported)

       Inline math: $x = y + z$

       Block math:
       $$\\sum_{i=1}^{n} x_i = x_1 + x_2 + \\cdots + x_n$$
       |]

sampleTypstFile :: Text
sampleTypstFile =
  [__i|\#set page(paper: "a4")
       \#set text(font: "Linux Libertine", size: 11pt)

       = Sample Typst Document

       This is a sample Typst file to test exporter functionality.

       == Features Demonstrated

       === Text Formatting

       - *Bold text*
       - _Italic text_
       - `Inline code`
       - \#strike[Strikethrough text]

       === Code Blocks

       ```python
       def hello_world():
           print("Hello, World!")
           return 42
       ```

       === Links and References

       \#link("https://example.com")[Link to example]

       === Tables

       \#table(
         columns: 3,
         [Column 1], [Column 2], [Column 3],
         [Value A], [Value B], [Value C],
         [Data 1], [Data 2], [Data 3]
       )

       === Lists

       1. Numbered list item
       2. Another numbered item
          - Nested bullet
          - Another nested bullet

       === Math

       Inline math: $x = y + z$

       Block math:
       $ sum_(i=1)^n i = (n(n+1))/2 $

       === Figures and Alignment

       \#align(center)[
         This text is centered
       ]

       === Custom Styling

       \#text(fill: blue)[This text is blue]

       \#rect(fill: gray.lighten(80%), inset: 8pt)[
         This is content in a gray box.
       ]
       |]

testExportIpynb :: (HasBaseContext ctx, HasNixEnvironment ctx, HasMaybeBubblewrap ctx) => Text -> FilePath -> ExampleT ctx IO ()
testExportIpynb = testExport "ipynb" $ \f -> A.encodeFile f sampleJupyterNotebook

testExportMd :: (HasBaseContext ctx, HasNixEnvironment ctx, HasMaybeBubblewrap ctx) => Text -> FilePath -> ExampleT ctx IO ()
testExportMd = testExport "md" $ \f -> T.writeFile f sampleMdFile

-- A notebook with a cell whose output (inline text and a verbatim file) rides its closing line
sampleLatexFile :: Text
sampleLatexFile =
  [__i|\\documentclass{article}
       \\usepackage{codedown}
       \\begin{document}
       \\section{Sample}

       \\begin{codedown}[folded]{python}
       x = {1: "%d" % 5}  \# braces { and \\end{itemize} are skipped
       \\end{codedown}\\codedownoutput{\\codedowntext{50\\% of \\{x\\}}\\codedowntextfile{out.txt}}

       \\end{document}
       |]

testExportLatex :: (HasBaseContext ctx, HasNixEnvironment ctx, HasMaybeBubblewrap ctx) => Text -> FilePath -> ExampleT ctx IO ()
testExportLatex = testExport "tex" $ \f -> do
  T.writeFile f sampleLatexFile
  T.writeFile (takeDirectory f </> "out.txt") "line one {\n  two % \\x\n"

testExportTypst :: (HasBaseContext ctx, HasNixEnvironment ctx, HasMaybeBubblewrap ctx) => Text -> FilePath -> ExampleT ctx IO ()
testExportTypst = testExport "typ" $ \f -> T.writeFile f sampleTypstFile

testExport :: (HasBaseContext ctx, HasNixEnvironment ctx, HasMaybeBubblewrap ctx) => FilePath -> (FilePath -> IO ()) -> Text -> FilePath -> ExampleT ctx IO ()
testExport inputExtension writeInputFile exporterName outputExtension = do
  nixEnv <- getContext nixEnvironment
  exporterInfo <- readExporterInfoByName (nixEnv </> "lib" </> "codedown" </> "exporters.yaml") exporterName

  Just dir <- getCurrentFolder

  let inputFile = dir </> ("input" <.> inputExtension)
  let outputFile = dir </> ("output" <.> outputExtension)

  liftIO $ writeInputFile inputFile

  (cmd:args) <- case exporterInfoArgs exporterInfo of
    [] -> expectationFailure [i|Couldn't get exporter info args|]
    xs -> return $ fmap T.unpack xs <> [inputFile, outputFile]

  cp <- getContext maybeBubblewrap >>= \case
    Nothing -> return ((proc cmd args) { cwd = Just dir })
    Just bwrapBinary -> do
      -- /bin/sh is needed: xelatex pipes its XDV to xdvipdfmx via popen, so PDF
      -- export fails with "I can't write on file" if sh can't exec. On a glibc
      -- distro (the ARC runner image) sh also needs its ELF interpreter, hence
      -- /lib and /lib64; -try because NixOS has neither.
      let bwrapArgs = ["--ro-bind", "/bin", "/bin"
                      , "--ro-bind-try", "/lib", "/lib"
                      , "--ro-bind-try", "/lib64", "/lib64"
                      , "--bind", dir, dir
                      , "--ro-bind", "/nix", "/nix"
                      , "--tmpfs", "/tmp"
                      , "--dev", "/dev"
                      , "--proc", "/proc"
                      , "--chdir", dir
                      , "--clearenv"
                      , "--setenv", "HOME", dir
                      , "--setenv", "TMPDIR", "/tmp"
                      ]
                      <> ["--"]
                      <> (cmd : args)

      return (proc bwrapBinary bwrapArgs)

  void $ readCreateProcessWithLogging cp ""

  doesPathExist outputFile >>= \case
    True -> return ()
    False -> expectationFailure [i|Expected path to exist: '#{outputFile}'|]

readExporterInfos :: MonadIO m => FilePath -> m [ExporterInfo]
readExporterInfos exportersYaml =
  liftIO (A.decodeFileStrict exportersYaml) >>= \case
    Nothing -> expectationFailure [i|Couldn't decode '#{exportersYaml}'|]
    Just x -> pure x

readExporterInfoByName :: MonadIO m => FilePath -> Text -> m ExporterInfo
readExporterInfoByName exportersYaml desired = do
  exporterInfos <- readExporterInfos exportersYaml
  case headMay [x | x@(ExporterInfo {..}) <- exporterInfos, exporterInfoName == desired] of
    Nothing -> expectationFailure [i|Couldn't find exporter info named '#{desired}'. (Had: #{fmap exporterInfoName exporterInfos})|]
    Just x -> pure x

-- | Start a live render on the export server behind an exporter and wait for its first output:
-- the serve-mode counterpart of 'testExport'. A wrapper that forgets to write the output, or
-- names a program that isn't on the server's PATH, fails here.
testExportServe :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Text -> FilePath -> LanguageKind -> Text -> Text -> [(Text, Text)] -> [(FilePath, B.ByteString)] -> ExampleT ctx m ()
testExportServe serverName filename languageKind code format options extraFiles = do
  sessionOptions <- lspSessionOptionsFor serverName filename languageKind code
  Helpers.withLspSession (sessionOptions { lspSessionOptionsExtraFiles = extraFiles }) $ \(LspSessionInfo {..}) -> do
    let uri = filePathToUri (lspSessionInfoHomeDir </> lspSessionInfoFileName)
    let startParams = A.object [
          "taskId" A..= ("test" :: Text)
          , "uri" A..= getUri uri
          , "format" A..= format
          , "options" A..= M.fromList options
          , "debounceMs" A..= (50 :: Int)
          ]
    void $ request SMethod_WorkspaceExecuteCommand (ExecuteCommandParams Nothing "codedown.render.start" (Just [startParams]))

    nextRenderOutput >>= \case
      A.Object (AKM.lookup "files" -> Just (A.Array files)) | not (Prelude.null files) -> return ()
      x -> expectationFailure [i|Expected a render output with files, got: #{A.encode x}|]

-- | The params of the next render output notification, skipping the log and status ones.
nextRenderOutput :: LspContext ctx m => Session (ExampleT ctx m) A.Value
nextRenderOutput = anyMessage >>= \case
  FromServerMess method msg -> case splitServerMethod method of
    IsServerEither -> case msg of
      NotMess (TNotificationMessage _ (SMethod_CustomMethod p) params)
        | T.pack (symbolVal p) == "codedown/render/output" -> return params
      _ -> nextRenderOutput
    _ -> nextRenderOutput
  _ -> nextRenderOutput
