{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Haskell (tests) where

import Control.Lens ((^.))
import Data.Aeson as A
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Vector as V
import Language.LSP.Test hiding (message)
import Language.LSP.Types
import Language.LSP.Types.Lens hiding (actions)
import Spec.Tests.Haskell.Common
import Spec.Tests.Haskell.Diagnostics
import Spec.Tests.Haskell.DocumentHighlight
import Spec.Tests.Haskell.Hover
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.JupyterTypes
import TestLib.NixEnvironmentContext
import TestLib.TestSearchers

#if MIN_VERSION_aeson(2,0,0)
#else
import qualified Data.HashMap.Strict        as HM
#endif


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
      haskellDiagnosticsTests lsName

      documentHighlightTests

      describe "statements" $ do
        it "doesn't choke on a single-line statement" $ do
          pending

        it "doesn't choke on a multi-line statement" $ do
          pending

      hoverTests

      it "symbols" $ doNotebookSession documentHighlightCode $ \filename -> do
        ident <- openDoc filename "haskell"
        Left documentSymbols <- getDocumentSymbols ident
        fmap (^. name) documentSymbols `shouldBe` ["foo"]

      describe "code actions" $ do
        it "gets no code actions for putStrLn" $ doNotebookSession documentHighlightCode $ \filename -> do
          ident <- openDoc filename "haskell"
          actions <- getCodeActions ident (Range (Position 1 0) (Position 1 8))
          actions `shouldBe` []

        it "gets code actions for foo" $ doNotebookSession documentHighlightCode $ \filename -> do
          ident <- openDoc filename "haskell"
          actions <- getCodeActions ident (Range (Position 1 0) (Position 1 8))
          actions `shouldBe` []


main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
