
module Spec.Tests.Java.LSP.WorkspaceSymbols (tests) where

import Data.Text (Text)
import Language.LSP.Protocol.Types
import Language.LSP.Test
import qualified Language.LSP.Test.Helpers as Helpers
import Spec.Tests.Java.Common
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits (waitUntil)
import TestLib.LSP
import TestLib.Types


-- | Guards includeSourceMethodDeclarations: JDT LS answers a workspace query with types
-- alone by default, and a cell's declarations all end up as methods on the projection's
-- synthetic class.
tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Workspace symbols" $ do
  it "finds a method, not just its class" $ doSession' "test.java" lsName code $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName LanguageKind_Java
    _ <- getDocumentSymbols ident

    waitUntil projectTimeout $ do
      symbols <- symbolNames <$> getWorkspaceSymbols "alpha"
      symbols `listShouldContain` "alphaHelper"
      symbols `listShouldContain` "AlphaThing"

code :: Text
code = "class AlphaThing {\n  int alphaHelper() {\n    return 42;\n  }\n}\n"
