{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Searchers (tests) where

import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.TestSearchers
import TestLib.Types


tests :: SimpleSpec
tests = describe "Searchers" $ do
  it "searcher has some results" $ testSearcherHasNonemptyResults "packageSearch"

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions $
  introduceBootstrapNixpkgs $
  tests
