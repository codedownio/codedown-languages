
module Spec.Tests.Rust.LSP.WorkspaceSymbols (tests) where

import Control.Lens ((^.))
import Data.String.Interpolate
import qualified Data.Text as T
import Language.LSP.Protocol.Lens (name)
import Language.LSP.Protocol.Types
import Language.LSP.Test
import qualified Language.LSP.Test.Helpers as Helpers
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits (waitUntil)
import TestLib.LSP
import TestLib.Types


-- | Guards the all_symbols search kind set in the rust-analyzer config: on rust-analyzer's
-- default this query answers with alpha_thing and not alpha_helper.
tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Workspace symbols" $ do
  it "finds a cell's function as well as its type" $ doNotebookSession "rust-analyzer" code $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName "rust"
    _ <- getDocumentSymbols ident

    waitUntil 300 $ do
      symbols <- symbolNames <$> getWorkspaceSymbols "alpha"
      symbols `listShouldContain` "alpha_helper"
      symbols `listShouldContain` "alpha_thing"

  where
    code = [__i|struct alpha_thing { x: i32 }

                fn alpha_helper() -> i32 { 42 }
               |]

symbolNames :: ([SymbolInformation] |? ([WorkspaceSymbol] |? Null)) -> [T.Text]
symbolNames (InL symbolInformations) = fmap (^. name) symbolInformations
symbolNames (InR (InL workspaceSymbols)) = fmap (^. name) workspaceSymbols
symbolNames (InR (InR Null)) = []
