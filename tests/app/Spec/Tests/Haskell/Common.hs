{-# LANGUAGE CPP #-}

module Spec.Tests.Haskell.Common where

import Data.String.Interpolate
import Data.Text as T
import TestLib.NixTypes


lsName :: Text
lsName = "haskell-language-server"

kernelSpec :: Text -> NixKernelSpec
kernelSpec ghcPackage = NixKernelSpec {
  nixKernelName = "haskell"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just [i|Haskell (#{ghcPackage})|]
  , nixKernelPackages = [nameOnly "aeson", nameOnly "bytestring"]
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelExtraConfig = Just [
      [i|ghcPackage = "#{ghcPackage}"|]
      , "settings.lsp.haskell-language-server.debug = true"
      -- , "settings.lsp.haskell-language-server.super-debug = true"
      ]
  }

kernelSpecWithHlintOutput :: Text -> NixKernelSpec
kernelSpecWithHlintOutput ghcPackage = (kernelSpec ghcPackage) {
  nixKernelExtraConfig = Just [
      "settings.enableHlintOutput = true"
      ]
  }
