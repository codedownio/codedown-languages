{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Spec.Tests.Julia (tests) where

import Control.Lens hiding (List)
import Data.Aeson as A
import Data.String.Interpolate
import Data.Text as T
import Language.LSP.Protocol.Lens hiding (diagnostics, hover, text)
import Language.LSP.Protocol.Types
import Language.LSP.Test hiding (message)
import Spec.Tests.Julia.Diagnostics
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Util


tests :: TopSpec
tests = do
  juliaTests "julia16"
  juliaTests "julia18"
  juliaTests "julia19"

juliaTests :: Text -> TopSpec
juliaTests lang = describe [i|Julia (#{lang})|] $ introduceNixEnvironment [kernelSpec lang] [] [i|Julia (#{lang})|] $ introduceJupyterRunner $ do
  testKernelSearchersNonempty lang

  testKernelStdout lang [i|println("hi")|] "hi\n"

  describe "LSP" $ do
    diagnosticsTests lsName

    itHasHoverSatisfying lsName "test.jl" Nothing [__i|print("hi")|] (Position 0 2) $ \hover -> do
      let InL (MarkupContent MarkupKind_Markdown text) = hover ^. contents
      text `textShouldContain` "Write to `io` (or to the default output stream"

    it "highlights foo" $ doNotebookSession lsName documentHighlightCode $ \filename -> do
      ident <- openDoc filename "haskell"
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
kernelSpec lang = NixKernelSpec {
  nixKernelName = lang
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just [i|Julia (#{lang})|]
  , nixKernelPackages = [
      nameOnly "JSON3"
      , nameOnly "Plots"
      , nameOnly "Roots"
      ]
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Just $ aesonFromList [
      ("lsp.LanguageServer.enable", A.Bool True)
      , ("lsp.LanguageServer.debug", A.Bool True)
      , ("lsp.LanguageServer.index", A.Bool True)
      ]
  }

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
