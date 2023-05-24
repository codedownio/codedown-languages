{-# LANGUAGE CPP #-}

module Spec.Tests.Haskell.Common where

import Data.Aeson as A
import Data.String.Interpolate
import Data.Text as T
import TestLib.NixTypes

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap          as HM
#else
import qualified Data.HashMap.Strict        as HM
#endif

lsName :: Text
lsName = "haskell-language-server"

kernelSpec :: Text -> NixKernelSpec
kernelSpec lang = NixKernelSpec {
  nixKernelName = lang
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just [i|Haskell (#{lang})|]
  , nixKernelPackages = [nameOnly "aeson", nameOnly "bytestring"]
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Just $ HM.fromList [
      ("haskell-language-server.debug", A.Bool True)
      ]
  }

kernelSpecWithHlintOutput :: Text -> NixKernelSpec
kernelSpecWithHlintOutput lang = (kernelSpec lang) {
  nixKernelSettings = Just $ HM.fromList [
      ("enableHlintOutput", A.Bool True)
      ]
  }
