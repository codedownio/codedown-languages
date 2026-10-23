{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Spec.Tests.Julia (tests) where

import Data.String.Interpolate
import Data.Text as T
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types

import qualified Spec.Tests.Julia.LSP as LSP
import qualified Spec.Tests.Julia.VariableInspector as VariableInspector


tests :: LanguageSpec
tests = do
  juliaTests "julia_110"
  juliaTests "julia_110-bin"

  juliaTests "julia_111"
  juliaTests "julia_111-bin"

  juliaTests "julia_112"
  juliaTests "julia_112-bin"

kernelName :: Text -> Text
kernelName _juliaPackage = "julia"

juliaTests :: Text -> LanguageSpec
juliaTests juliaPackage = describe [i|Julia (#{juliaPackage})|] $ introduceNixEnvironment [kernelSpec juliaPackage] [] [i|Julia (#{juliaPackage})|] $ introduceJupyterRunner $ do
  testKernelSearchersNonempty (kernelName juliaPackage)
  testHasExpectedFields (kernelName juliaPackage)

  testKernelStdout (kernelName juliaPackage) [i|println("hi")|] "hi\n"

  VariableInspector.tests (kernelName juliaPackage)

  LSP.tests

kernelSpec :: Text -> NixKernelSpec
kernelSpec juliaPackage = NixKernelSpec {
  nixKernelName = kernelName juliaPackage
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just [i|Julia (#{juliaPackage})|]
  , nixKernelPackages = [
      nameOnly "JSON3"
      , nameOnly "Plots"
      , nameOnly "Roots"
      ]
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelExtraConfig = Just [
      "lsp.LanguageServer.enable = true"
      , "lsp.LanguageServer.debug = true"
      , "lsp.LanguageServer.index = true"
      , [i|juliaPackage = "#{juliaPackage}"|]
      ]
  }

main :: IO ()
main = jupyterMain tests
