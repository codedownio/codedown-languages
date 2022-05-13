
module Spec.Tests.Python3Kernel where

import Test.Sandwich


tests :: TopSpec
tests = describe "Python 3 kernel" $ do
  it "tests addition" $ do
    2 `shouldBe` 2
