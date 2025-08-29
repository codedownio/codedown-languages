{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Coq (tests) where

import Data.Aeson
import Data.String.Interpolate
import Data.Text (Text)
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types


kernelSpec :: Text -> NixKernelSpec
kernelSpec coqPackages = NixKernelSpec {
  nixKernelName = "coq"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "Coq"
  , nixKernelPackages = [nameOnly "bignums"]
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelExtraConfig = Just [
      [i|coqPackages = "#{coqPackages}"|]
      ]
  }

tests :: LanguageSpec
tests = do
  tests' "coqPackages_8_20" -- Current default

  -- TODO: https://github.com/codedownio/codedown-languages/issues/75
  -- tests' "coqPackages" -- This is 9.0 on master


tests' :: Text -> LanguageSpec
tests' coqPackages = describe [i|Coq (#{coqPackages})|] $ introduceNixEnvironment [kernelSpec coqPackages] [] "Coq" $ introduceJupyterRunner $ do
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
