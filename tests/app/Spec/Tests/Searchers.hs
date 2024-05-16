{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Searchers (tests) where

import Test.Sandwich as Sandwich
import TestLib.TestSearchers


tests :: TopSpec
tests = describe "Searchers" $ do
  it "searcher has some results" $ testSearcherHasNonemptyResults ".#searcher"

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
