{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Spec.Tests.Typst (tests) where

import Control.Lens hiding (List)
import Control.Monad
import Data.String.Interpolate
import Data.Text as T
import Language.LSP.Protocol.Lens hiding (diagnostics, hover, text)
import Language.LSP.Protocol.Types
import Language.LSP.Test hiding (message)
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestBuilding
import TestLib.TestSearchers
import TestLib.Types


kernelName :: Text
kernelName = "typst"

tests :: LanguageSpec
tests = describe [i|Typst|] $ introduceNixEnvironment [] config [i|Typst|] $ introduceJupyterRunner $ do
  it "has expected fields" $ do
    testEval [i|exporters.typst.settingsSchema|]
    testEval [i|exporters.typst.modes|]
    testEval [i|exporters.typst.settings|]
    testEval [i|exporters.typst.args|]
    testEval [i|exporters.typst.meta|]

    -- Used to view all versions in codedown-languages
    testEval [i|exporters.typst.versions|]

  it "searcher builds" $ do
    void $ testBuild [i|exporters.typst.packageSearch|]

  describe "LSP" $ do
    testDiagnosticsLabelDesired "simple" lsName "test.typ" (Just "typst")
      [__i|\#loremz(45)|]
      ((== [(Range (Position 0 1) (Position 0 7), Nothing, "unknown variable: loremz")]) . getDiagnosticRanges')


documentHighlightCode :: Text
documentHighlightCode = [__i|foo = "hello"
                             println(foo)|]

documentHighlightResults :: [DocumentHighlight]
documentHighlightResults = [
  DocumentHighlight (Range (Position 0 0) (Position 0 3)) (Just DocumentHighlightKind_Write)
  , DocumentHighlight (Range (Position 1 8) (Position 1 11)) (Just DocumentHighlightKind_Read)
  ]

lsName :: Text
lsName = "tinymist"

-- kernelSpec :: NixKernelSpec
-- kernelSpec = NixKernelSpec {
--   nixKernelName = "typst"
--   , nixKernelChannel = "codedown"
--   , nixKernelDisplayName = Just [i|Typst|]
--   , nixKernelPackages = []
--   , nixKernelMeta = Nothing
--   , nixKernelIcon = Nothing
--   , nixKernelExtraConfig = Just [
--       "lsp.tinymist.enable = true"
--       ]
--   }

config = [
  "exporters.typst.enable = true;"
  , "exporters.typst.lsp.tinymist.enable = true;"
  ]

main :: IO ()
main = jupyterMain tests
