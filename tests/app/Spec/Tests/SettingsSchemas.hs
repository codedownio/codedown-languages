{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.SettingsSchemas (tests) where

import Control.Monad.IO.Class
import Data.Aeson as A
import qualified Data.Map as M
import Data.Maybe
import Data.String.Interpolate
import Data.Text
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.TestBuilding
import TestLib.Types
import TestLib.Util


type PackageName = Text
type TargetName = Text
type AllSettingsSchemas = M.Map PackageName (M.Map TargetName A.Object)

tests :: SimpleSpec
tests = describe "Settings schemas" $ do
  it "all settings schema items have required fields" $ do
    allSettingsSchemasFile <- testBuildUsingFlake ".#allSettingsSchemas"
    info [i|Got file: #{allSettingsSchemasFile}|]

    Right (byPackage :: AllSettingsSchemas) <- liftIO $ A.eitherDecodeFileStrict allSettingsSchemasFile

    itemsLackingField byPackage "defaultValue" `shouldBe` []

    info [i|Num lacking title: #{Prelude.length (itemsLackingField byPackage "title")}|]
    itemsLackingField byPackage "title" `shouldBe` []

itemsLackingField :: AllSettingsSchemas -> Text -> [Text]
itemsLackingField byPackage field = catMaybes [
  classify packageName targetName item
  | (packageName, m) <- M.toList byPackage
  , (targetName, item) <- M.toList m
  ]
  where
    classify packageName targetName item = case item of
      (aesonLookup field -> Just _) -> Nothing
      _ -> Just ([i|#{packageName}.#{targetName}|] :: Text)

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions $
  introduceBootstrapNixpkgs $
  tests
