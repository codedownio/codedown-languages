module Spec.Tests.R.LSP.Completion (tests) where

import Control.Lens
import Data.String.Interpolate
import Data.Text (Text)
import Language.LSP.Protocol.Lens (label)
import Language.LSP.Protocol.Types
import Language.LSP.Test
import qualified Language.LSP.Test.Helpers as Helpers
import Spec.Tests.R.Common
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits (waitUntil)
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Completions" $ do
  it "completes base functions and local variables (main.ipynb)" $ doNotebookSession lsName code $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName LanguageKind_R

    waitUntil 60 $ do
      labels <- fmap (^. label) <$> getCompletions ident (Position 1 2)
      info [i|Got completions: #{labels}|]
      labels `listShouldContain` "sqrt"
      labels `listShouldContain` "sqVariable"

  it "completes base functions and local variables (test.R)" $ doSession' "test.R" lsName code $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName LanguageKind_R

    waitUntil 60 $ do
      labels <- fmap (^. label) <$> getCompletions ident (Position 1 2)
      info [i|Got completions: #{labels}|]
      labels `listShouldContain` "sqrt"
      labels `listShouldContain` "sqVariable"

code :: Text
code = "sqVariable <- 42\nsq"
