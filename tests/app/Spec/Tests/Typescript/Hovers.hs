{-# LANGUAGE OverloadedStrings #-}

module Spec.Tests.Typescript.Hovers (tests) where

import Data.String.Interpolate
import Data.Text (Text)
import Language.LSP.Protocol.Types
import Language.LSP.Test
import qualified Language.LSP.Test.Helpers as Helpers
import Spec.Tests.Typescript.Common
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits (waitUntil)
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Hovers" $ do
  it "hovers a typed local" $ doSession' "test.ts" lsName localCode $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName LanguageKind_TypeScript

    waitUntil 60 $ do
      hover <- getHoverOrException ident (Position 1 8)
      allHoverText hover `textShouldContain` [i|number|]

  it "hovers a function from an environment package" $ doSession' "test.ts" lsName d3Code $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName LanguageKind_TypeScript

    waitUntil 60 $ do
      hover <- getHoverOrException ident (Position 1 17)
      allHoverText hover `textShouldContain` [i|ScaleLinear|]

localCode :: Text
localCode = [__i|function double(x: number): number { return x * 2; }
                 const y = double(21);|]

d3Code :: Text
d3Code = [__i|import * as d3 from "d3";
              const s = d3.scaleLinear();|]
