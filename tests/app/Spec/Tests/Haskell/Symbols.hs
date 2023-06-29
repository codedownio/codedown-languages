
module Spec.Tests.Haskell.Symbols where

import Control.Lens ((^.))
import Language.LSP.Protocol.Lens hiding (actions)
import Language.LSP.Test hiding (message)
import Spec.Tests.Haskell.Common
import Spec.Tests.Haskell.DocumentHighlight (documentHighlightCode)
import Test.Sandwich as Sandwich
import TestLib.LSP


symbolsTests :: (LspContext context m) => SpecFree context m ()
symbolsTests = describe "Symbols" $ do
  it "symbols" $ doNotebookSession lsName documentHighlightCode $ \filename -> do
    ident <- openDoc filename "haskell"
    Right documentSymbols <- getDocumentSymbols ident
    fmap (^. name) documentSymbols `shouldBe` ["foo"]
