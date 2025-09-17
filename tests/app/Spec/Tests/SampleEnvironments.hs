{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.SampleEnvironments (tests) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson as A
import qualified Data.List as L
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Yaml as Yaml
import System.FilePath
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.NixTypes
import TestLib.TH
import TestLib.TestBuilding
import TestLib.Util
import UnliftIO.Directory


tests :: TopSpec
tests = describe "Sample environments" $ introduceBootstrapNixpkgs $ introduceJustBubblewrap $ do
  parallelN 4 $
    forM_ (L.sort fileList) $ \file -> do
      describe [i|#{file}|] $ do
        it "Builds" $ do
          let name = T.dropEnd 4 (T.pack file) -- Drop the .nix suffix
          built <- testBuildUsingFlake [i|.\#sample_environment_#{name}|]
          info [i|Got built: #{built}|]

        it "Has well-formed UI metadata" $ do
          let name = T.dropEnd 4 (T.pack file) -- Drop the .nix suffix

          envRoot <- testBuildUsingFlake [i|.\#sample_environment_#{name}|]

          yamlPath <- testBuildUsingFlake [i|.\#sample_environment_#{name}.ui_metadata_yaml|]
          NixHydrationResult {..} <- liftIO (Yaml.decodeFileEither yamlPath) >>= \case
            Left err -> expectationFailure [i|Couldn't decode UI metadata YAML: #{err}|]
            Right x -> pure x
          info [i|packages: #{A.encode nixHydrationResultPackages}|]

          forM_ (M.toList nixHydrationResultPackages) (\(n, v) -> validatePackage envRoot n v)

        it "Has a good kernelspec" $ do
          let name = T.dropEnd 4 (T.pack file) -- Drop the .nix suffix

          envRoot <- testBuildUsingFlake [i|.\#sample_environment_#{name}|]
          let kernelsDir = envRoot </> "lib" </> "codedown" </> "kernels"

          kernels <- doesDirectoryExist kernelsDir >>= \case
            False -> return []
            True -> listDirectory kernelsDir
          info [i|kernels: #{kernels}|]

          forM_ kernels $ \kernel -> do
            let kernelFile = kernelsDir </> kernel </> "kernel.json"
            info [i|Looking at kernel: #{kernelFile}|]

            Just (contents :: A.Object) <- liftIO $ A.decodeFileStrict kernelFile
            case aesonLookup "language" contents of
              Just (A.String lang) -> info [i|Kernel #{kernel} --> #{lang}|]
              x -> expectationFailure [i|Expected kernel #{kernel} to contain a language, but it was: #{x}|]

validatePackage :: (MonadLoggerIO m, MonadFail m) => FilePath -> Text -> NixPackage -> m ()
validatePackage envRoot attr (NixPackage {nixPackageMeta=(NixMeta {..}), ..}) = do
  when ("shells." `T.isPrefixOf` attr) $ do
    info [i|(#{nixPackageName}) Shell detected; checking it has a mainProgram|]
    shouldBeJust nixMetaMainProgram
    Just program <- return nixMetaMainProgram
    info [i|(#{nixPackageName}) Checking for #{envRoot </> "bin" </> program}|]
    doesPathExist (envRoot </> "bin" </> program) >>= (`shouldBe` True)

  shouldBeJust nixMetaIcon
  shouldBeJust nixMetaIconMonochrome

fileList :: [String]
fileList = $(getFileListRelativeToRoot "sample_environments")

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
