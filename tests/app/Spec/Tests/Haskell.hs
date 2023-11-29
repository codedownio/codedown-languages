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
  -- See languages/haskell/default.nix for details on what's available

  -- haskellCommonTests "haskell-ghc810"
  haskellCommonTests "haskell-ghc90"
  haskellCommonTests "haskell-ghc92"
  haskellCommonTests "haskell-ghc94"
  haskellCommonTests "haskell-ghc96"


haskellCommonTests :: Text -> LanguageSpec
haskellCommonTests lang = do
  describe [i|Haskell #{lang} with hlint output|] $ introduceNixEnvironment [kernelSpecWithHlintOutput lang] [] "Haskell" $ do
    describe "Kernel" $ do
      -- With the setting turned on, we should get hlint output
      itHasDisplayTexts lang etaExpandCode [Just (A.Array $ V.fromList [
                                                     String "Line 7: Eta reduce\n"
                                                     , String "Found:\n"
                                                     , String "baz2 x = baz x\n"
                                                     , String "Why not:\n"
                                                     , String "baz2 = baz"
                                                     ])]

  describe [i|Haskell #{lang}|] $ introduceNixEnvironment [kernelSpec lang] [] "Haskell" $ do
    introduceJupyterRunner $ do
      testKernelSearchersNonempty lang

      describe "Kernel" $ do
        itHasDisplayDatas lang [__i|putStrLn "hi"|] [M.fromList [(MimeType "text/plain", A.Array (V.fromList [A.String "hi"]))]]

        itHasDisplayDatas lang [__i|:info String|] [stringInfo]

        -- We shouldn't get hlint output by default
        itHasDisplayDatas lang etaExpandCode []

    describe "LSP" $ do
      codeActionsTests
      diagnosticsTests lsName
      documentHighlightTests
      hoverTests
      statementsTests
      symbolsTests


main :: IO ()
main = jupyterMain tests
