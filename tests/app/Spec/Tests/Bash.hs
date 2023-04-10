{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Bash (tests) where

import Data.String.Interpolate
import Language.LSP.Types
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers


kernelSpec :: NixKernelSpec
kernelSpec = NixKernelSpec {
  nixKernelChannel = "codedown"
  , nixKernelName = "bashInteractive"
  , nixKernelDisplayName = Just "Bash"
  , nixKernelPackages = []
  , nixKernelLanguageServers = [
      nameOnly "bash-language-server"
      ]
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Nothing
  }

tests :: TopSpec
tests = describe "Bash" $ introduceNixEnvironment [kernelSpec] [] "Bash" $ introduceJupyterRunner $ do
  testKernelSearchersBuild "bashInteractive"

  testKernelStdout "bash" [i|echo hi|] "hi\n"

  -- testDiagnostics "shellcheck" "test.sh" [__i|FOO=42
  --                                            |] $ \diagnostics -> do
  --   assertDiagnosticRanges diagnostics []

  testDiagnostics "bash-language-server" "test.sh" [__i|FOO=42|] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics [
      (Range (Position 0 0) (Position 0 3), Just (InL 2034))
      ]

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
