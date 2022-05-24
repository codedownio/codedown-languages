module Main where

import Test.Sandwich

import qualified Spec.Tests as Tests

tests :: TopSpec
tests = Tests.tests


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
