
module Spec.Tests.Spellchecker (tests) where

import Control.Lens
import Data.String.Interpolate
import Language.LSP.Types
import Language.LSP.Types.Lens
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes


otherPackages = [
  channelAndAttr "codedown" "spellchecker"
  ]

tests :: TopSpec
tests = describe "Spellchecker" $ introduceNixEnvironment [] otherPackages "Python 3" $ do
  testDiagnostics "spellchecker" "test.md" [i|\# This is mispelled|] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics [(Range (Position 0 10) (Position 0 19), Nothing)]

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
