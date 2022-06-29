{-# LANGUAGE RankNTypes #-}

module TestLib.Lang.HaskellCommon where

import Data.Aeson as A
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text
import qualified Data.Vector as V
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.JupyterTypes
import TestLib.NixEnvironmentContext
import TestLib.NixTypes


haskellCommonTests :: Text -> TopSpec
haskellCommonTests lang = describe [i|Haskell #{lang}|] $ introduceNixEnvironment [kernelSpec] [] "Haskell" $ introduceJupyterRunner $ do
  testNotebookDisplayDataOutputs lang [__i|putStrLn "hi"|] [M.fromList [(MimeType "text/plain", A.Array (V.fromList [A.String "hi"]))]]

  where
    kernelSpec = NixKernelSpec {
      nixKernelChannel = "codedown"
      , nixKernelLanguage = lang
      , nixKernelDisplayName = Just "Haskell"
      , nixKernelPackages = []
      , nixKernelLanguageServers = []
      , nixKernelExtraJupyterConfig = Nothing
      , nixKernelMeta = Nothing
      , nixKernelIcon = Nothing
      , nixKernelSettingsSchema = Nothing
      , nixKernelSettings = Nothing
      }
