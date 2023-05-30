{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Rust (tests) where

import Data.Aeson as A
import Data.String.Interpolate
import Language.LSP.Test
import Language.LSP.Types
import Spec.Tests.Rust.Diagnostics
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Util


tests :: TopSpec
tests = describe "Rust" $ introduceNixEnvironment [kernelSpec] [] "Rust" $ introduceJupyterRunner $ do
  testKernelSearchersBuild "rust"

  testKernelStdout "rust" [__i|println!("hi")|] "hi\n"

  describe "LSP" $ do
    it "hovers println!" $ doNotebookSession "rust-analyzer" [i|println!("hi")|] $ \filename -> do
      ident <- openDoc filename "haskell"

      rsp <- getCustomRequest "rust-analyzer/analyzerStatus" $ A.toJSON ident
      info [i|Rust analyzer status: #{rsp}|]

      hover <- getHoverOrException ident (Position 0 1)
      allHoverText hover `textShouldContain` [i|Prints to the standard output|]

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
