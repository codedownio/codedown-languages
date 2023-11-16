{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Bash (tests) where

import Data.Aeson as A
import Data.String.Interpolate
import Language.LSP.Protocol.Types
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Util


kernelSpec :: NixKernelSpec
kernelSpec = NixKernelSpec {
  nixKernelChannel = "codedown"
  , nixKernelName = "bash"
  , nixKernelDisplayName = Just "Bash"
  , nixKernelPackages = []
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Just $ aesonFromList [
      ("lsp.bash-language-server.enable", A.Bool True)
      ]
  }

tests :: TopSpec
tests = describe "Bash" $ introduceNixEnvironment [kernelSpec] [] "Bash" $ introduceJupyterRunner $ do
  testKernelSearchersBuild "bash"

  testKernelStdout "bash" [i|echo hi|] "hi\n"

  -- testDiagnostics "shellcheck" "test.sh" Nothing [__i|FOO=42
  --                                            |] $ \diagnostics -> do
  --   assertDiagnosticRanges diagnostics []

  testDiagnostics "bash-language-server" "test.sh" Nothing [__i|FOO=42|] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics [
      (Range (Position 0 0) (Position 0 3), Just (InR "SC2034"))
      ]

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
