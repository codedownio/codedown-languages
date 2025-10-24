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
import qualified Data.Text.IO as T
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

  introduceNixEnvironment [] [pandocConfig] [i|Exporters (pandoc)|] $ do
    it "codedown-exporter-slidy" $ testExportMd "codedown-exporter-slidy" "html"
    it "codedown-exporter-beamer" $ testExportMd "codedown-exporter-beamer" "html"

  introduceNixEnvironment [] [typstConfig] [i|Exporters (typst)|] $ do
    it "codedown-exporter-typst" $ testExportTypst "codedown-exporter-typst" "pdf"


testTexliveScheme :: Text -> LanguageSpec
testTexliveScheme scheme = do
  introduceNixEnvironment [] [nbconvertConfig scheme] [i|Exporters (nbconvert, #{scheme})|] $ do
    it "codedown-exporter-asciidoc" $ testExportIpynb "codedown-exporter-asciidoc" "asciidoc"
    it "codedown-exporter-latex" $ testExportIpynb "codedown-exporter-latex" "tex"
    it "codedown-exporter-pdf" $ testExportIpynb "codedown-exporter-pdf" "pdf"
    it "codedown-exporter-html" $ testExportIpynb "codedown-exporter-html" "html"
    it "codedown-exporter-rst" $ testExportIpynb "codedown-exporter-rst" "rst"
    it "codedown-exporter-markdown" $ testExportIpynb "codedown-exporter-markdown" "md"
    -- it "codedown-exporter-slides" $ testExport "codedown-exporter-slides" "slides.html"

testExportIpynb :: (HasBaseContext ctx, HasNixEnvironment ctx) => Text -> FilePath -> ExampleT ctx IO ()
testExportIpynb = testExport "ipynb" $ \f -> A.encodeFile f sampleJupyterNotebook

testExportMd :: (HasBaseContext ctx, HasNixEnvironment ctx) => Text -> FilePath -> ExampleT ctx IO ()
testExportMd = testExport "md" $ \f -> T.writeFile f sampleMdFile

testExportTypst :: (HasBaseContext ctx, HasNixEnvironment ctx) => Text -> FilePath -> ExampleT ctx IO ()
testExportTypst = testExport "typ" $ \f -> T.writeFile f sampleTypstFile

testExport :: (HasBaseContext ctx, HasNixEnvironment ctx) => FilePath -> (FilePath -> IO ()) -> Text -> FilePath -> ExampleT ctx IO ()
testExport inputExtension writeInputFile exporterName outputExtension = do
  nixEnv <- getContext nixEnvironment
  exporterInfo <- readExporterInfoByName (nixEnv </> "lib" </> "codedown" </> "exporters.yaml") exporterName

  Just dir <- getCurrentFolder

  let inputFile = dir </> ("input" <.> inputExtension)
  let outputFile = dir </> ("output" <.> outputExtension)

  liftIO $ writeInputFile inputFile

  cp <- case exporterInfoArgs exporterInfo of
    [] -> expectationFailure [i|Couldn't get exporter info args|]
    (x:xs) -> return $ proc (T.unpack x) (fmap T.unpack xs <> [inputFile, outputFile])
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

nbconvertConfig :: Text -> Text
nbconvertConfig scheme = [__i|exporters.nbconvert.enable = true;
                              exporters.nbconvert.texliveScheme = "#{scheme}";|]

pandocConfig :: Text
pandocConfig = [__i|exporters.pandoc.enable = true;|]

typstConfig :: Text
typstConfig = [__i|exporters.typst.enable = true;|]
