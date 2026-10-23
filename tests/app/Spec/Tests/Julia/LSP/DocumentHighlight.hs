module Spec.Tests.Julia.LSP.DocumentHighlight (tests) where

import Data.String.Interpolate
import Data.Text (Text)
import Language.LSP.Protocol.Types
import Language.LSP.Test hiding (message)
import qualified Language.LSP.Test.Helpers as Helpers
import Spec.Tests.Julia.Common
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = it "highlights foo" $ doNotebookSession lsName documentHighlightCode $ \(Helpers.LspSessionInfo {..}) -> do
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
