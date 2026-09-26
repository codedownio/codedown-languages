{-# LANGUAGE OverloadedStrings #-}

module Spec.Tests.Javascript.Completion (tests) where

import Control.Lens
import Control.Monad.IO.Unlift
import qualified Data.List as L
import Data.String.Interpolate
import Data.Text (Text)
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Types
import Language.LSP.Test
import qualified Language.LSP.Test.Helpers as Helpers
import Spec.Tests.Javascript.Common
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits (waitUntil)
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Completions" $ do
  it "provides local variable completions" $ doSession' "test.js" lsName localVarCode $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName LanguageKind_JavaScript

    waitUntil 60 $ do
      completions <- getCompletions ident (Position 2 2)
      info [i|Got completions: #{completions}|]
      let labels = fmap (^. label) completions
      labels `listShouldContain` "myVariable"
      labels `listShouldContain` "myFloat"

  -- This one is the interesting case: it only works because the language server is pointed at
  -- the environment's node_modules. Without that, d3 is `any` and these completions are just
  -- the words already in the file.
  it "provides completions from an environment package" $ doSession' "test.js" lsName d3Code $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName LanguageKind_JavaScript

    waitUntil 60 $ do
      completions <- getCompletions ident (Position 1 3)
      info [i|Got completions: #{completions}|]
      let labels = fmap (^. label) completions
      labels `listShouldContain` "scaleLinear"
      labels `listShouldContain` "scaleBand"

localVarCode :: Text
localVarCode = [__i|const myVariable = 42;
                    const myFloat = 3.14;
                    my|]

d3Code :: Text
d3Code = [__i|const d3 = require("d3");
              d3.|]

listShouldContain :: (MonadIO m, Eq a, Show a) => [a] -> a -> m ()
listShouldContain haystack needle = case L.elem needle haystack of
  True -> return ()
  False -> expectationFailure [i|Expected list to contain #{show needle}, but had: #{show haystack}|]
