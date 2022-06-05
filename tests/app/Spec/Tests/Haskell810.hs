
module Spec.Tests.Haskell810 (tests) where

import Data.String.Interpolate
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.Lang.HaskellCommon (haskellCommonTests)
import TestLib.NixEnvironmentContext
import TestLib.NixTypes


lang = "haskell-ghc8107"

tests :: TopSpec
tests = haskellCommonTests lang

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
