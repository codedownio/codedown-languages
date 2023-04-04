{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Searchers (tests) where

import Test.Sandwich as Sandwich
import TestLib.TestSearchers


tests :: TopSpec
tests = describe "Searchers" $ do
  it "languages searcher has some results" $ testSearcherHasNonemptyResults ".#languagesSearcher"
  it "exporters searcher has some results" $ testSearcherHasNonemptyResults ".#exportersSearcher"
  it "shells searcher has some results" $ testSearcherHasNonemptyResults ".#shellsSearcher"

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
