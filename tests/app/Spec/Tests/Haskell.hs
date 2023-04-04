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
import Spec.Tests.Haskell.Symbols
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.JupyterTypes
import TestLib.NixEnvironmentContext
import TestLib.TestSearchers


tests :: TopSpec
tests = do
  -- See languages/haskell/default.nix for details on what's available

  -- haskellCommonTests "haskell-ghc865"
  -- haskellCommonTests "haskell-ghc884"

  haskellCommonTests "haskell-ghc8107"
  haskellCommonTests "haskell-ghc902"
  haskellCommonTests "haskell-ghc924"

  -- haskellCommonTests "haskell-ghc942"

haskellCommonTests :: Text -> TopSpec
haskellCommonTests lang = do
  describe [i|Haskell #{lang} with hlint output|] $ introduceNixEnvironment [kernelSpecWithHlintOutput lang] [] "Haskell" $ introduceJupyterRunner $ do
    describe "Kernel" $ do
      -- With the setting turned on, we should get hlint output
      itHasDisplayTexts lang etaExpandCode [Just (A.Array $ V.fromList [
                                                     String "Line 7: Eta reduce\n"
                                                     , String "Found:\n"
                                                     , String "baz2 x = baz x\n"
                                                     , String "Why not:\n"
                                                     , String "baz2 = baz"
                                                     ])]

  describe [i|Haskell #{lang}|] $ introduceNixEnvironment [kernelSpec lang] [] "Haskell" $ introduceJupyterRunner $ do
    testKernelSearchers lang

    describe "Kernel" $ do
      itHasDisplayDatas lang [__i|putStrLn "hi"|] [M.fromList [(MimeType "text/plain", A.Array (V.fromList [A.String "hi"]))]]

      -- We shouldn't get hlint output by default
      itHasDisplayDatas lang etaExpandCode []

    describe "LSP" $ do
      codeActionsTests
      diagnosticsTests lsName
      documentHighlightTests
      symbolsTests

      describe "statements" $ do
        it "doesn't choke on a single-line statement" $ do
          pending

        it "doesn't choke on a multi-line statement" $ do
          pending

      hoverTests


main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
