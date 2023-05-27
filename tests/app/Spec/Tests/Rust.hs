{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Rust (tests) where

import Data.Aeson as A
import Data.String.Interpolate
import Spec.Tests.Rust.Diagnostics
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

  diagnosticsTests


kernelSpec :: NixKernelSpec
kernelSpec = NixKernelSpec {
  nixKernelName = "rust"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "Rust"
  , nixKernelPackages = []
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Just $ aesonFromList [
      ("lsp.rust-analyzer.debug", A.Bool True)
      ]
  }

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
