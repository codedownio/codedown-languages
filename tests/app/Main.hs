module Main where

import Test.Sandwich

import qualified Spec.Tests as Tests
import TestLib.Types


tests :: TopSpecWithOptions' SpecialOptions
tests = Tests.tests


main :: IO ()
main = runSandwichWithCommandLineArgs' defaultOptions specialOptions tests
