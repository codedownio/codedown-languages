{-# LANGUAGE CPP #-}

module Spec.Tests.Haskell.Common (
  lsName
  , kernelSpec
  , kernelSpecWithHlintOutput
  , etaExpandCode

  , ghcPackage
  , HasGhcPackage
  , introduceGhcPackage
  , skippedOnGhc910
  ) where

import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Data.Function
import Data.Maybe
import Data.String.Interpolate
import Data.Text as T
import Test.Sandwich
import TestLib.NixTypes


lsName :: Text
lsName = "haskell-language-server"

kernelSpec :: Text -> NixKernelSpec
kernelSpec ghcPkg = NixKernelSpec {
  nixKernelName = "haskell"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just [i|Haskell (#{ghcPkg})|]
  , nixKernelPackages = [nameOnly "aeson", nameOnly "bytestring"]
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelExtraConfig = Just [
      [i|ghcPackage = "#{ghcPkg}"|]
      , "lsp.haskell-language-server.debug = true"
      -- , "lsp.haskell-language-server.super-debug = true"
      ]
  }

kernelSpecWithHlintOutput :: Text -> NixKernelSpec
kernelSpecWithHlintOutput ghcPkg = baseKernelSpec {
  nixKernelExtraConfig = nixKernelExtraConfig baseKernelSpec
                       & fromMaybe []
                       & ("enableHlintOutput = true" :)
                       & Just
  }
  where
    baseKernelSpec = kernelSpec ghcPkg

-- | Shared between the kernel's hlint output test and the LSP eta-reduce diagnostic.
etaExpandCode :: Text
etaExpandCode = [__i|module Foo where

                     baz :: Int -> Int
                     baz x = x + 1

                     baz2 :: Int -> Int
                     baz2 x = baz x
                    |]

-- * The GHC version under test

-- Test discovery calls a no-argument 'tests', so the LSP specs take the GHC version from
-- the context rather than as a parameter.

ghcPackage :: Label "ghcPackage" Text
ghcPackage = Label

type HasGhcPackage context = HasLabel context "ghcPackage" Text

introduceGhcPackage :: (
  Monad m
  ) => Text -> SpecFree (LabelValue "ghcPackage" Text :> context) m () -> SpecFree context m ()
introduceGhcPackage pkg = introduce' nodeOpts [i|GHC package #{pkg}|] ghcPackage (return pkg) (const (return ()))

skippedOnGhc910 :: (
  HasGhcPackage context, MonadIO m
  ) => SpecFree context m () -> SpecFree context m ()
skippedOnGhc910 = before' nodeOpts "Skip on ghc910" $ do
  pkg <- getContext ghcPackage
  when (pkg == "ghc910") $ pendingWith "skipped on ghc910"

nodeOpts :: NodeOptions
nodeOpts = defaultNodeOptions {
  nodeOptionsRecordTime = False
  , nodeOptionsCreateFolder = False
  }
