{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Spellchecker (tests) where

import Control.Lens
import Data.String.Interpolate
import Data.Text
import Language.LSP.Protocol.Lens hiding (actions, diagnostics)
import Language.LSP.Protocol.Types
import Language.LSP.Test hiding (message)
import Test.Sandwich as Sandwich
import Test.Sandwich.Contexts.Waits (waitUntil)
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes


otherPackages :: [ChannelAndAttr]
otherPackages = [
  channelAndAttr "codedown" "spellchecker"
  ]

tests :: TopSpec
tests = describe "Spellchecker" $ introduceNixEnvironment [] otherPackages "Spellchecker env" $ do
  testDiagnostics "spellchecker" "test.md" Nothing [i|\# This is mispelled|] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics [(Range (Position 0 10) (Position 0 19), Nothing)]

  testDiagnostics "spellchecker" "test.md" Nothing [i|I've done a thing.|] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics []

  it "has a code action to fix the misspelling" $ doNotebookSession "spellchecker" [i|\# This is mispelled|] $ \filename -> do
    ident <- openDoc filename "spellchecker"
    actions <- getCodeActions ident (Range (Position 0 0) (Position 0 19))
    waitUntil 60 $ do
      fmap getTitle actions `shouldBe` ["foo"]

getTitle :: (HasTitle a Text, HasTitle b Text) => (a |? b) -> Text
getTitle (InL x) = x ^. title
getTitle (InR x) = x ^. title

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
