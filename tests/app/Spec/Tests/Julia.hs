{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Spec.Tests.Julia (tests) where

import Control.Lens hiding (List)
import Data.String.Interpolate
import Data.Text as T
import Language.LSP.Test hiding (message)
import Language.LSP.Types
import Language.LSP.Types.Lens hiding (diagnostics, hover, text)
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers

import Data.Aeson as A
import TestLib.Util


tests :: TopSpec
tests = do
  juliaTests "julia"
  juliaTests "julia16"
  juliaTests "julia18"

juliaTests :: Text -> TopSpec
juliaTests lang = describe [i|Julia (#{lang})|] $ introduceNixEnvironment [kernelSpec lang] [] [i|Julia (#{lang})|] $ introduceJupyterRunner $ do
  testKernelSearchersNonempty lang

  testKernelStdout lang [i|println("hi")|] "hi\n"

  describe "LSP" $ do
    testDiagnostics lsName "test.jl" (Just "julia") [__i|using JSON3
                                                         printlnzzzz("HI")
                                                        |] $ \diagnostics -> do
      assertDiagnosticRanges' diagnostics [(Range (Position 1 0) (Position 1 11), Nothing, "Missing reference: printlnzzzz")]

    testDiagnostics lsName "test.jl" (Just "julia") [__i|using Plots
                                                         xx = range(0, 10, length=100)
                                                         y = sin.(xx)
                                                         plot(xx, y)
                                                         printlnzzzz("HI")
                                                        |] $ \diagnostics -> do
      assertDiagnosticRanges' diagnostics [(Range (Position 4 0) (Position 4 11), Nothing, "Missing reference: printlnzzzz")]

    testDiagnostics lsName "test.jl" (Just "julia") [i|printlnzzzz("HI")|] $ \diagnostics -> do
      assertDiagnosticRanges' diagnostics [(Range (Position 0 0) (Position 0 11), Nothing, "Missing reference: printlnzzzz")]

    itHasHoverSatisfying lsName "test.jl" Nothing [__i|print("hi")|] (Position 0 2) $ \hover -> do
      let HoverContents (MarkupContent MkMarkdown text) = hover ^. contents
      text `textShouldContain` "Write to `io` (or to the default output stream"

    it "highlights foo" $ doNotebookSession lsName documentHighlightCode $ \filename -> do
      ident <- openDoc filename "haskell"
      getHighlights ident (Position 0 1) >>= (`shouldBe` List documentHighlightResults)


documentHighlightCode :: Text
documentHighlightCode = [__i|foo = "hello"
                             println(foo)|]

documentHighlightResults :: [DocumentHighlight]
documentHighlightResults = [
  DocumentHighlight (Range (Position 0 0) (Position 0 3)) (Just HkWrite)
  , DocumentHighlight (Range (Position 1 8) (Position 1 11)) (Just HkRead)
  ]

lsName :: Text
lsName = "LanguageServer"

kernelSpec :: Text -> NixKernelSpec
kernelSpec lang = NixKernelSpec {
  nixKernelName = lang
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just [i|Julia (#{lang})|]
  , nixKernelPackages = [nameOnly "JSON3", nameOnly "Plots"]
  , nixKernelLanguageServers = [nameOnly "LanguageServer"]
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Just $ aesonFromList [
      ("LanguageServer.debug", A.Bool True)
      ]
  }

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
