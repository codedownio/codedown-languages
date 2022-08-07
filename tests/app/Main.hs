module Main where

import Test.Sandwich

import qualified Spec.Tests as Tests

tests :: TopSpecWithOptions' Tests.SpecialOptions
tests = Tests.tests


main :: IO ()
main = runSandwichWithCommandLineArgs' defaultOptions Tests.specialOptions tests
