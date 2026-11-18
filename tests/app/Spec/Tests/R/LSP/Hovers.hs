module Spec.Tests.R.LSP.Hovers (tests) where

import Data.String.Interpolate
import Data.Text (Text)
import Language.LSP.Protocol.Types
import Language.LSP.Test
import qualified Language.LSP.Test.Helpers as Helpers
import Spec.Tests.R.Common
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits (waitUntil)
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Hovers" $ do
  it "hovers a base function (main.ipynb)" $ doNotebookSession lsName code $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName LanguageKind_R

    waitUntil 60 $ do
      hover <- getHoverOrException ident (Position 0 6)
      allHoverText hover `textShouldContain` [i|square root of x|]

  it "hovers a base function (test.R)" $ doSession' "test.R" lsName code $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName LanguageKind_R

    waitUntil 60 $ do
      hover <- getHoverOrException ident (Position 0 6)
      allHoverText hover `textShouldContain` [i|square root of x|]

code :: Text
code = "y <- sqrt(16)\n"
