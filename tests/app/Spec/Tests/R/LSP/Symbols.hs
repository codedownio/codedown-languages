module Spec.Tests.R.LSP.Symbols (tests) where

import Control.Lens ((^.))
import Data.String.Interpolate
import Data.Text (Text)
import Language.LSP.Protocol.Lens (name)
import Language.LSP.Protocol.Types
import Language.LSP.Test
import qualified Language.LSP.Test.Helpers as Helpers
import Spec.Tests.R.Common
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits (waitUntil)
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Symbols" $ do
  it "outlines a function defined in a cell" $ doNotebookSession lsName code $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName LanguageKind_R

    waitUntil 60 $ do
      names <- getDocumentSymbols ident >>= \case
        Right documentSymbols -> pure (fmap (^. name) documentSymbols)
        Left symbolInformations -> pure (fmap (^. name) symbolInformations)
      names `listShouldContain` "doubled"

code :: Text
code = "doubled <- function(v) {\n  v * 2\n}\n"
