
module Spec.Tests.Java.LSP.Symbols (tests) where

import Control.Lens ((^.))
import Data.String.Interpolate
import Data.Text (Text)
import Language.LSP.Protocol.Lens (name)
import Language.LSP.Protocol.Types
import Language.LSP.Test
import qualified Language.LSP.Test.Helpers as Helpers
import Spec.Tests.Java.Common
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits (waitUntil)
import TestLib.LSP
import TestLib.Types


-- | Everything a cell declares hangs off the projection's synthetic class, which covers
-- lines no cell owns. jnls used to drop that node and the cell's symbols with it.
tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Symbols" $ do
  it "outlines a method declared in a cell" $ doNotebookSession lsName code $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName LanguageKind_Java

    waitUntil projectTimeout $ do
      getDocumentSymbols ident >>= \case
        Right documentSymbols -> fmap (^. name) documentSymbols `listShouldContain` "doubled(int)"
        Left symbolInformations -> expectationFailure [i|Expected a symbol tree, got: #{symbolInformations}|]

code :: Text
code = "int doubled(int v) {\n    return v * 2;\n}\n"
