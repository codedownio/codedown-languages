{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Coq (tests) where

import Data.Aeson
import Data.String.Interpolate
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types


kernelSpec :: NixKernelSpec
kernelSpec = NixKernelSpec {
  nixKernelName = "coq"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "Coq"
  , nixKernelPackages = [nameOnly "bignums"]
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Nothing
  }

tests :: LanguageSpec
tests = describe "Coq" $ introduceNixEnvironment [kernelSpec] [] "Coq" $ introduceJupyterRunner $ do
  testKernelSearchersBuild "coq"
  testHasExpectedFields "coq"

  itHasExecuteTexts "coq" [__i|Require Import Bignums.BigN.BigN.
                               Check (BigN.add_comm 1 2).|] [Just $ Array [
                                                                String "BigN.add_comm 1 2\n"
                                                                , String "     : (1 + 2 == 2 + 1)%bigN"
                                                                ]
                                                            ]

  itHasExecuteTexts "coq" [i|Print true.|] [Just $ Array [
                                               String "Inductive bool : Set :=  true : bool | false : bool."
                                               ]]

  -- testDiagnostics "coq-lsp-server" "test.v" Nothing [i||] $ \diagnostics -> do
  --   assertDiagnosticRanges diagnostics []


main :: IO ()
main = jupyterMain tests
