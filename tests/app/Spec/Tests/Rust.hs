{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Rust (tests) where

import Data.Aeson as A
import Data.String.Interpolate
import qualified Data.Text as T
import Safe
import Spec.Tests.Rust.Diagnostics
import Spec.Tests.Rust.Hovers
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Util


tests :: TopSpec
tests = describe "Rust" $ introduceNixEnvironment [kernelSpec] [] "Rust" $ introduceJupyterRunner $ do
  testKernelSearchersBuild "rust"

  testKernelStdout "rust" [__i|println!("hi")|] "hi\n"

  testKernelStdoutCallback "rust" randCode $ \output -> do
    case readMay (T.unpack (T.strip output)) of
      Just (x :: Int) | x >= 0 && x < 256 -> return ()
      _ -> expectationFailure [i|Unexpected output: #{show output}|]

  describe "LSP" $ do
    hoverTests
    diagnosticsTests


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
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
