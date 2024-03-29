{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Rust (tests) where

import Data.Aeson as A
import Data.String.Interpolate
import qualified Data.Text as T
import Safe
import Spec.Tests.Rust.Changes
import Spec.Tests.Rust.Completion
import Spec.Tests.Rust.Diagnostics
import Spec.Tests.Rust.Hovers
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types
import TestLib.Util


tests :: LanguageSpec
tests = describe "Rust" $ introduceNixEnvironment [kernelSpec] [] "Rust" $ introduceJupyterRunner $ do
  testKernelSearchersBuild "rust"
  testHasExpectedFields "rust"

  testKernelStdout "rust" [__i|println!("hi")|] "hi\n"

  testKernelStdoutCallback "rust" randCode $ \case
    Just t -> case readMay (T.unpack (T.strip t)) of
      Just (x :: Int) | x >= 0 && x < 256 -> return ()
      _ -> expectationFailure [i|Unexpected output: #{show t}|]
    Nothing -> expectationFailure [i|Kernel produced no output.|]

  describe "LSP" $ do
    changesTests
    completionTests
    diagnosticsTests
    hoverTests

-- We need a sleep to make this test reliable. It seems the kernel has a problem where
-- it can exit before flushing stdout?
-- See TODO: file issue on evcxr
randCode :: T.Text
randCode = [__i|use rand::prelude::*;
                let x: u8 = random();
                println!("{}", x);|]

kernelSpec :: NixKernelSpec
kernelSpec = NixKernelSpec {
  nixKernelName = "rust"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "Rust"
  , nixKernelPackages = [nameOnly "rand"]
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Just $ aesonFromList [
      ("lsp.rust-analyzer.debug", A.Bool True)
      ]
  }

main :: IO ()
main = jupyterMain tests
