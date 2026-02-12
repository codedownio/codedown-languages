module Spec.Tests.Go.Hovers (tests) where

import Data.String.Interpolate
import Data.Text (Text)
import Language.LSP.Protocol.Types
import Language.LSP.Test
import qualified Language.LSP.Test.Helpers as Helpers
import Spec.Tests.Go.Common
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits (waitUntil)
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Hovers" $ do
  describe "main.ipynb" $ do
    it "hovers function call (main.ipynb)" $ doSession' "main.ipynb" lsName sqrtCodeNotebook $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName LanguageKind_Go

      waitUntil 60 $ do
        hover <- getHoverOrException ident (Position 1 15)
        allHoverText hover `textShouldContain` [i|Sqrt|]

  describe "test.go" $ do
    it "hovers function call (test.go)" $ doSession' "test.go" lsName sqrtCodeStandalone $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName LanguageKind_Go

      waitUntil 60 $ do
        hover <- getHoverOrException ident (Position 3 19)
        allHoverText hover `textShouldContain` [i|Sqrt|]

sqrtCodeNotebook :: Text
sqrtCodeNotebook = [__i|import "math"
                        result := math.Sqrt(16.0)
                        _ = result|]

sqrtCodeStandalone :: Text
sqrtCodeStandalone = [__i|package main
                          import "math"
                          func main() {
                              result := math.Sqrt(16.0)
                              _ = result
                          }|]
