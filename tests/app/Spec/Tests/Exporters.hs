{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Exporters (tests) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Aeson as A
import Data.String.Interpolate
import Data.Text as T
import Safe
import System.FilePath
import Test.Sandwich as Sandwich
import TestLib.Exporters
import TestLib.NixEnvironmentContext
import TestLib.Types
import UnliftIO.Directory
import UnliftIO.Process


tests :: LanguageSpec
tests = describe "Exporters" $ do
  -- testTexliveScheme "scheme-minimal"
  -- testTexliveScheme "scheme-basic"
  -- testTexliveScheme "scheme-small"
  -- testTexliveScheme "scheme-medium"
  -- testTexliveScheme "scheme-bookpub"
  -- testTexliveScheme "scheme-tetex"

  testTexliveScheme "scheme-full"


testTexliveScheme :: Text -> LanguageSpec
testTexliveScheme scheme = introduceNixEnvironment [] [otherConfig scheme] [i|Exporters (#{scheme})|] $ do
  it "codedown-exporter-asciidoc" $ testExport "codedown-exporter-asciidoc" "asciidoc"
  it "codedown-exporter-latex" $ testExport "codedown-exporter-latex" "tex"
  it "codedown-exporter-pdf" $ testExport "codedown-exporter-pdf" "pdf"
  it "codedown-exporter-html" $ testExport "codedown-exporter-html" "html"
  it "codedown-exporter-rst" $ testExport "codedown-exporter-rst" "rst"
  -- it "codedown-exporter-slides" $ testExport "codedown-exporter-slides" "slides.html"
  it "codedown-exporter-markdown" $ testExport "codedown-exporter-markdown" "md"

  -- it "codedown-exporter-slidy" $ testExport "codedown-exporter-beamer" "html"
  -- it "codedown-exporter-beamer" $ testExport "codedown-exporter-beamer" "html"
  -- it "codedown-exporter-beamer" $ testExport "codedown-exporter-beamer" "html"

testExport :: (HasBaseContext ctx, HasNixEnvironment ctx) => Text -> FilePath -> ExampleT ctx IO ()
testExport name extension = do
  nixEnv <- getContext nixEnvironment
  pdfExporterInfo <- readExporterInfoByName (nixEnv </> "lib" </> "codedown" </> "exporters.yaml") name

  Just dir <- getCurrentFolder

  let inputFile = dir </> "input.ipynb"
  let outputFile = dir </> ("output" <.> extension)

  liftIO $ A.encodeFile inputFile sampleJupyterNotebook

  cp <- case exporterInfoArgs pdfExporterInfo of
    [] -> expectationFailure [i|Couldn't get exporter info args|]
    (x:xs) -> return $ proc (T.unpack x) (fmap T.unpack xs <> [inputFile, outputFile])
  info [i|cp: #{cp}|]
  void $ readCreateProcessWithLogging (cp { cwd = Just dir }) ""

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

otherConfig :: Text -> Text
otherConfig scheme = [__i|exporters.nbconvert.enable = true;
                          exporters.nbconvert.texliveScheme = "#{scheme}";|]
