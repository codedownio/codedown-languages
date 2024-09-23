{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Haskell (tests) where

import Data.Aeson as A
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Vector as V
import Spec.Tests.Haskell.CodeActions
import Spec.Tests.Haskell.Common
import Spec.Tests.Haskell.Diagnostics
import Spec.Tests.Haskell.DocumentHighlight
import Spec.Tests.Haskell.Hover
import Spec.Tests.Haskell.Info
import Spec.Tests.Haskell.Statements
import Spec.Tests.Haskell.Symbols
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.JupyterTypes
import TestLib.NixEnvironmentContext
import TestLib.TestSearchers
import TestLib.Types


tests :: LanguageSpec
tests = do
  -- See kernels/haskell/default.nix for details on what's available

  -- haskellCommonTests "haskell-ghc810"
  -- haskellCommonTests "haskell-ghc90"
  haskellCommonTests "ghc92"
  haskellCommonTests "ghc94"
  haskellCommonTests "ghc96"
  haskellCommonTests "ghc98"

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

        itHasDisplayDatas (kernelName ghcPackage) [__i|:info String|] [stringInfo]

        -- We shouldn't get hlint output by default
        itHasDisplayDatas (kernelName ghcPackage) etaExpandCode []

    describe "LSP" $ do
      codeActionsTests
      diagnosticsTests ghcPackage lsName
      documentHighlightTests
      hoverTests
      statementsTests ghcPackage
      symbolsTests


main :: IO ()
main = jupyterMain tests
