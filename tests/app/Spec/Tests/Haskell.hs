{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Haskell (tests) where

import Data.Aeson as A
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Vector as V
import Spec.Tests.Haskell.Common
import Spec.Tests.Haskell.Info
import Test.Sandwich as Sandwich
import TestLib.Contexts.GhcPackage (introduceGhcPackage)
import TestLib.JupyterRunnerContext
import TestLib.JupyterTypes
import TestLib.NixEnvironmentContext
import TestLib.TestSearchers
import TestLib.Types

import qualified Spec.Tests.Haskell.LSP as LSP


tests :: LanguageSpec
tests = do
  -- See kernels/haskell/default.nix for details on what's available

  -- haskellCommonTests "ghc810"
  -- haskellCommonTests "ghc90"
  -- haskellCommonTests "ghc92"
  -- haskellCommonTests "ghc94"
  haskellCommonTests "ghc96"
  haskellCommonTests "ghc98"
  haskellCommonTests "ghc910"
  haskellCommonTests "ghc912"

kernelName :: Text -> Text
kernelName _ghcPackage = "haskell"

haskellCommonTests :: Text -> LanguageSpec
haskellCommonTests ghcPackage = do
  describe [i|Haskell #{ghcPackage} with hlint output|] $ introduceNixEnvironment [kernelSpecWithHlintOutput ghcPackage] [] "Haskell" $ do
    describe "Kernel" $ do
      -- With the setting turned on, we should get hlint output
      itHasDisplayTexts (kernelName ghcPackage) etaExpandCode [
        Just (A.Array $ V.fromList [
                 String "Line 7: Eta reduce\n"
                 , String "Found:\n"
                 , String "baz2 x = baz x\n"
                 , String "Why not:\n"
                 , String "baz2 = baz"
                 ])]

  describe [i|Haskell #{ghcPackage}|] $ introduceNixEnvironment [kernelSpec ghcPackage] [] "Haskell" $ do
    introduceJupyterRunner $ do
      testKernelSearchersNonempty (kernelName ghcPackage)
      testHasExpectedFields (kernelName ghcPackage)

      describe "Kernel" $ do
        itHasDisplayDatas (kernelName ghcPackage) [__i|putStrLn "hi"|] [M.fromList [(MimeType "text/plain", A.Array (V.fromList [A.String "hi"]))]]

        itHasDisplayDatas (kernelName ghcPackage) [__i|:info String|] [stringInfo ghcPackage]

        -- We shouldn't get hlint output by default
        itHasDisplayDatas (kernelName ghcPackage) etaExpandCode []

    introduceGhcPackage ghcPackage $
      LSP.tests


main :: IO ()
main = jupyterMain tests
