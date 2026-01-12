
module Spec.Tests.Haskell.Symbols (tests) where

import Control.Lens ((^.))
import Language.LSP.Protocol.Lens hiding (actions)
import Language.LSP.Test hiding (message)
import qualified Language.LSP.Test.Helpers as Helpers
import Spec.Tests.Haskell.Common
import Spec.Tests.Haskell.DocumentHighlight (documentHighlightCode)
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.Types
import UnliftIO.Timeout


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Symbols" $ do
  it "symbols" $ doNotebookSession lsName documentHighlightCode $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName "haskell"
    Just (Right documentSymbols) <- timeout 300_000_000 $ getDocumentSymbols ident
    fmap (^. name) documentSymbols `shouldBe` ["foo"]
