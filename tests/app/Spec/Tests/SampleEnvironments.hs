{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.SampleEnvironments (tests) where

import Data.String.Interpolate
import Test.Sandwich as Sandwich
import TestLib.TestBuilding


tests :: TopSpec
tests = describe "Sample environments farm" $ do
  it "builds the sample environments farm" $ do
    testBuild [i|.\#sample_environments_farm|]

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
