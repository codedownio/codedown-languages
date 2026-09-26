{-# LANGUAGE OverloadedStrings #-}

module Spec.Tests.Typescript.Completion (tests) where

import Control.Lens
import Control.Monad.IO.Unlift
import qualified Data.List as L
import Data.String.Interpolate
import Data.Text (Text)
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Types
import Language.LSP.Test
import qualified Language.LSP.Test.Helpers as Helpers
import Spec.Tests.Typescript.Common
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits (waitUntil)
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Completions" $ do
  it "completes members of a typed value" $ doSession' "test.ts" lsName interfaceCode $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName LanguageKind_TypeScript

    waitUntil 60 $ do
      completions <- getCompletions ident (Position 2 2)
      info [i|Got completions: #{completions}|]
      let labels = fmap (^. label) completions
      labels `listShouldContain` "width"
      labels `listShouldContain` "height"

  it "completes from an environment package" $ doSession' "test.ts" lsName d3Code $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName LanguageKind_TypeScript

    waitUntil 60 $ do
      completions <- getCompletions ident (Position 1 3)
      info [i|Got completions: #{completions}|]
      let labels = fmap (^. label) completions
      labels `listShouldContain` "scaleLinear"
      labels `listShouldContain` "scaleBand"

interfaceCode :: Text
interfaceCode = [__i|interface Box { width: number; height: number }
                     const b: Box = { width: 1, height: 2 };
                     b.|]

d3Code :: Text
d3Code = [__i|import * as d3 from "d3";
              d3.|]

listShouldContain :: (MonadIO m, Eq a, Show a) => [a] -> a -> m ()
listShouldContain haystack needle = case L.elem needle haystack of
  True -> return ()
  False -> expectationFailure [i|Expected list to contain #{show needle}, but had: #{show haystack}|]
