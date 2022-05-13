module Main where

import Test.Sandwich

import qualified Spec.Tests as Tests

tests :: TopSpec
tests = describe "Initial test" $ do
  it "tests addition" $ do
    2 `shouldBe` 2

  Tests.tests


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
