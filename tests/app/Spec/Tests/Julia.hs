{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Spec.Tests.Julia (tests) where

import Control.Monad
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
  juliaTests True "julia_112"
  juliaTests True "julia_112-bin"

  -- LanguageServer.jl doesn't run on Julia 1.13 yet, and Plots needs a newer General registry
  juliaTests False "julia_113"
  juliaTests False "julia_113-bin"

kernelName :: Text -> Text
kernelName _juliaPackage = "julia"

juliaTests :: Bool -> Text -> LanguageSpec
juliaTests hasLanguageServer juliaPackage = describe [i|Julia (#{juliaPackage})|] $ introduceNixEnvironment [kernelSpec hasLanguageServer juliaPackage] [] [i|Julia (#{juliaPackage})|] $ introduceJupyterRunner $ do
  testKernelSearchersNonempty (kernelName juliaPackage)
  testHasExpectedFields (kernelName juliaPackage)

  testKernelStdout (kernelName juliaPackage) [i|println("hi")|] "hi\n"

  VariableInspector.tests (kernelName juliaPackage)

  when hasLanguageServer LSP.tests

kernelSpec :: Bool -> Text -> NixKernelSpec
kernelSpec hasLanguageServer juliaPackage = NixKernelSpec {
  nixKernelName = kernelName juliaPackage
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just [i|Julia (#{juliaPackage})|]
  , nixKernelPackages = nameOnly "JSON3" : [p | hasLanguageServer, p <- [nameOnly "Plots", nameOnly "Roots"]]
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
