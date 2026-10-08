{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Bash (tests) where

import Data.String.Interpolate
import Language.LSP.Protocol.Types
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types

import qualified Spec.Tests.Bash.VariableInspector as VariableInspector


kernelSpec :: NixKernelSpec
kernelSpec = NixKernelSpec {
  nixKernelChannel = "codedown"
  , nixKernelName = "bash"
  , nixKernelDisplayName = Just "Bash"
  , nixKernelPackages = []
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelExtraConfig = Just [
      "lsp.bash-language-server.enable = true"
      ]
  }

tests :: LanguageSpec
tests = describe "Bash" $ introduceNixEnvironment [kernelSpec] [] "Bash" $ introduceJupyterRunner $ do
  testKernelSearchersBuild "bash"
  testHasExpectedFields "bash"

  testKernelStdout "bash" [i|echo hi|] "hi\n"

  VariableInspector.tests "bash"

  -- testDiagnostics "shellcheck" "test.sh" Nothing [__i|FOO=42
  --                                            |] $ \diagnostics -> do
  --   assertDiagnosticRanges diagnostics []

  testDiagnostics "bash-language-server" "test.sh" LanguageKind_ShellScript [__i|FOO=42|] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics [
      (Range (Position 0 0) (Position 0 3), Just (InR "SC2034"))
      ]

  itFormatsAs "bash-language-server" "test.sh" LanguageKind_ShellScript
    "if [ 1 = 1 ]; then\necho   hi\n   fi\n"
    "if [ 1 = 1 ]; then\n  echo hi\nfi\n"

main :: IO ()
main = jupyterMain tests
