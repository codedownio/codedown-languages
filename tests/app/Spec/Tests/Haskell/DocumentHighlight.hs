
module Spec.Tests.Haskell.DocumentHighlight where

import Data.String.Interpolate
import Data.Text as T
import Language.LSP.Test hiding (message)
import Language.LSP.Types
import Spec.Tests.Haskell.Common
import Test.Sandwich as Sandwich
import TestLib.LSP


documentHighlightTests :: (LspContext context m) => SpecFree context m ()
documentHighlightTests = describe "Document highlight" $ do
  it "highlights foo" $ doNotebookSession lsName documentHighlightCode $ \filename -> do
    ident <- openDoc filename "haskell"
    getHighlights ident (Position 0 1) >>= (`shouldBe` List documentHighlightResults)

documentHighlightCode :: Text
documentHighlightCode = [__i|foo = "hello"
                             putStrLn foo|]

documentHighlightResults :: [DocumentHighlight]
documentHighlightResults = [
  DocumentHighlight (Range (Position 0 0) (Position 0 3)) (Just HkWrite)
  , DocumentHighlight (Range (Position 1 9) (Position 1 12)) (Just HkRead)
  ]
