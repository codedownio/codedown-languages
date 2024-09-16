{-# LANGUAGE CPP #-}

module Spec.Tests.Haskell.Common where

import Data.String.Interpolate
import Data.Text as T
import TestLib.NixTypes


lsName :: Text
lsName = "haskell-language-server"

kernelSpec :: Text -> NixKernelSpec
kernelSpec lang = NixKernelSpec {
  nixKernelName = lang
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just [i|Haskell (#{lang})|]
  , nixKernelPackages = [nameOnly "aeson", nameOnly "bytestring"]
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelExtraConfig = Just [
      "settings.lsp.haskell-language-server.debug = true"
      -- , "settings.lsp.haskell-language-server.super-debug = true"
      ]
  }

kernelSpecWithHlintOutput :: Text -> NixKernelSpec
kernelSpecWithHlintOutput lang = (kernelSpec lang) {
  nixKernelExtraConfig = Just [
      "settings.enableHlintOutput = true"
      ]
  }
