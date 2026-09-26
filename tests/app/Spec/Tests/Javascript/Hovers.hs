{-# LANGUAGE OverloadedStrings #-}

module Spec.Tests.Javascript.Hovers (tests) where

import Data.String.Interpolate
import Data.Text (Text)
import Language.LSP.Protocol.Types
import Language.LSP.Test
import qualified Language.LSP.Test.Helpers as Helpers
import Spec.Tests.Javascript.Common
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits (waitUntil)
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Hovers" $ do
  it "hovers a local function" $ doSession' "test.js" lsName localCode $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName LanguageKind_JavaScript

    waitUntil 60 $ do
      hover <- getHoverOrException ident (Position 1 11)
      allHoverText hover `textShouldContain` [i|double|]

  -- Typed rather than `any`, which is the signal that the package's @types resolved.
  it "hovers a function from an environment package" $ doSession' "test.js" lsName d3Code $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName LanguageKind_JavaScript

    waitUntil 60 $ do
      hover <- getHoverOrException ident (Position 1 14)
      allHoverText hover `textShouldContain` [i|ScaleLinear|]

localCode :: Text
localCode = [__i|function double(x) { return x * 2; }
                 const y = double(21);|]

d3Code :: Text
d3Code = [__i|const d3 = require("d3");
              const s = d3.scaleLinear();|]
