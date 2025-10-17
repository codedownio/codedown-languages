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
  it "Has exporter metadata" $ do
    nixEnv <- getContext nixEnvironment
    pdfExporterInfo <- readExporterInfoByName (nixEnv </> "lib" </> "codedown" </> "exporters.yaml") "codedown-exporter-pdf"

    Just dir <- getCurrentFolder

    let inputFile = dir </> "input.ipynb"
    let outputFile = dir </> "input.pdf"

    liftIO $ A.encodeFile inputFile sampleJupyterNotebook

    cp <- case exporterInfoArgs pdfExporterInfo of
      [] -> expectationFailure [i|Couldn't get exporter info args|]
      (x:xs) -> return $ proc (T.unpack x) (fmap T.unpack xs <> [inputFile])
    info [i|cp: #{cp}|]
    void $ readCreateProcessWithLogging (cp { cwd = Just dir }) ""

    doesPathExist outputFile >>= (`shouldBe` True)

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
