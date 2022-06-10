
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
    assertDiagnosticRanges diagnostics []

    -- (Range (Position 3 8) (Position 3 8), Just (InR "W292"))
    -- (Range (Position 3 0) (Position 3 8), Just (InR "E303"))

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
