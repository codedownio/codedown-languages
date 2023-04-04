{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Spellchecker (tests) where

import Data.String.Interpolate
import Language.LSP.Types
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes


otherPackages :: [ChannelAndAttr]
otherPackages = [
  channelAndAttr "codedown" "spellchecker"
  ]

tests :: TopSpec
tests = describe "Spellchecker" $ introduceNixEnvironment [] otherPackages "Python 3" $ do
  testDiagnostics "spellchecker" "test.md" [i|\# This is mispelled|] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics [(Range (Position 0 10) (Position 0 19), Nothing)]

  testDiagnostics "spellchecker" "test.md" [i|I've done a thing.|] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics []

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
