{-# LANGUAGE RankNTypes #-}

module TestLib.Lang.HaskellCommon where

import Data.String.Interpolate
import Data.Text
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.NixEnvironmentContext
import TestLib.NixTypes


haskellCommonTests :: Text -> TopSpec
haskellCommonTests lang = describe [i|Haskell #{lang}|] $ introduceNixEnvironment [kernelSpec] [] "Haskell" $ introduceJupyterRunner $ do
  testKernelStdout lang [__i|putStrLn "hi"|] "hi\n"

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
