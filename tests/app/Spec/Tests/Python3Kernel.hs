
module Spec.Tests.Python3Kernel (tests) where

import Control.Monad
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Unlift
import Data.String.Interpolate
import Data.Text
import Test.Sandwich
import TestLib.NixEnvironmentContext
import TestLib.NixTypes


python3Environment = undefined

testKernelStdout :: MonadThrow m => Text -> Text -> Text -> SpecFree context m ()
testKernelStdout kernel code desired = it [i|#{kernel}: #{code} -> #{desired}|] $ do
  2 `shouldBe` 2


tests :: TopSpec
tests = introduceNixEnvironment python3Environment "Python 3" $ do
  it "gets the nix env" $ do
    env <- getContext nixEnvironment
    info [i|Got env: #{env}|]

  -- testKernelStdout "python" [i|print("hi")|]  "hi"

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
