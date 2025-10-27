{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module TestLib.Exporters where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson as A
import Data.Aeson.QQ.Simple
import Data.Aeson.TH as A
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Text.IO as T
import Safe
import System.FilePath
import Test.Sandwich as Sandwich
import TestLib.Aeson
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
      let bwrapArgs = ["--ro-bind", "/bin", "/bin" -- It seems to need /bin/sh
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
