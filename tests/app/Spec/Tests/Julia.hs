{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Spec.Tests.Julia (tests) where

import Control.Lens hiding (List)
import Data.String.Interpolate
import Data.Text as T
import Language.LSP.Protocol.Lens hiding (diagnostics, hover, text)
import Language.LSP.Protocol.Types
import Language.LSP.Test hiding (message)
import qualified Language.LSP.Test.Helpers as Helpers
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types

import qualified Spec.Tests.Julia.Diagnostics as Diagnostics


tests :: LanguageSpec
tests = do
  juliaTests "julia_19"
  juliaTests "julia_19-bin"

  juliaTests "julia_110"
  juliaTests "julia_110-bin"

  juliaTests "julia_111"
  juliaTests "julia_111-bin"

kernelName :: Text -> Text
kernelName _juliaPackage = "julia"

juliaTests :: Text -> LanguageSpec
juliaTests juliaPackage = describe [i|Julia (#{juliaPackage})|] $ introduceNixEnvironment [kernelSpec juliaPackage] [] [i|Julia (#{juliaPackage})|] $ introduceJupyterRunner $ do
  testKernelSearchersNonempty (kernelName juliaPackage)
  testHasExpectedFields (kernelName juliaPackage)

  testKernelStdout (kernelName juliaPackage) [i|println("hi")|] "hi\n"

  describe "LSP" $ do
    Diagnostics.tests lsName

    itHasHoverSatisfying lsName "test.jl" (LanguageKind_Custom "julia") [__i|print("hi")|] (Position 0 2) $ \hover -> do
      let InL (MarkupContent MarkupKind_Markdown text) = hover ^. contents
      text `textShouldContain` "Write to `io` (or to the default output stream"

    it "highlights foo" $ doNotebookSession lsName documentHighlightCode $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName "julia"
      getHighlights ident (Position 0 1) >>= (`shouldBe` documentHighlightResults)


documentHighlightCode :: Text
documentHighlightCode = [__i|foo = "hello"
                             println(foo)|]

documentHighlightResults :: [DocumentHighlight]
documentHighlightResults = [
  DocumentHighlight (Range (Position 0 0) (Position 0 3)) (Just DocumentHighlightKind_Write)
  , DocumentHighlight (Range (Position 1 8) (Position 1 11)) (Just DocumentHighlightKind_Read)
  ]

lsName :: Text
lsName = "LanguageServer"

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
