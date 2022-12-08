{-# LANGUAGE OverloadedLists #-}

module Spec.Tests.Coq (tests) where

import Control.Lens
import Data.Aeson
import Data.String.Interpolate
import Language.LSP.Types
import Language.LSP.Types.Lens
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers


kernelSpec = NixKernelSpec {
  nixKernelName = "coq"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "Coq"
  , nixKernelPackages = [nameOnly "ceres"]
  , nixKernelLanguageServers = [
      -- nameOnly "coq-language-server"
      ]
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Nothing
  }

tests :: TopSpec
tests = describe "Coq" $ introduceNixEnvironment [kernelSpec] [] "Coq" $ introduceJupyterRunner $ do
  testKernelSearchers "coq"

  itHasExecuteTexts "coq" [i|From Ceres Require Import Ceres.|] [Just $ Array []]

  itHasExecuteTexts "coq" [i|Print true.|] [Just $ Array [
                                               String "Inductive bool : Set :=  true : bool | false : bool."
                                               ]]

  -- testDiagnostics "coq-lsp-server" "test.v" [i||] $ \diagnostics -> do
  --   assertDiagnosticRanges diagnostics []


main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
