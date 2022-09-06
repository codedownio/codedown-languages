
module Spec.Tests.Haskell (tests) where

import Data.String.Interpolate
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.Lang.HaskellCommon (haskellCommonTests)
import TestLib.NixEnvironmentContext
import TestLib.NixTypes


tests :: TopSpec
tests = do
  -- See languages/haskell/default.nix for details on what's available

  -- haskellCommonTests "haskell-ghc865"
  -- haskellCommonTests "haskell-ghc884"

  haskellCommonTests "haskell-ghc8107"
  haskellCommonTests "haskell-ghc902"

  -- haskellCommonTests "haskell-ghc924"
  -- haskellCommonTests "haskell-ghc942"


main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
