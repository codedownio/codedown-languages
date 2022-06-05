
module Spec.Tests.Haskell92 (tests) where

import Data.String.Interpolate
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.Lang.HaskellCommon (haskellCommonTests)
import TestLib.NixEnvironmentContext
import TestLib.NixTypes


lang = "haskell-ghc922"

tests :: TopSpec
tests = haskellCommonTests lang

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
