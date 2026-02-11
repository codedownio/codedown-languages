module Spec.Tests.Go.Hovers (tests) where

import Control.Monad
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
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
  forM_ ["main.ipynb", "test.go"] $ \doc -> describe (T.unpack doc) $ do
    it [i|hovers function call (#{doc})|] $ doSession' doc lsName sqrtCode $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName LanguageKind_Go

      waitUntil 60 $ do
        hover <- getHoverOrException ident (Position 2 14)
        allHoverText hover `textShouldContain` [i|Sqrt|]


sqrtCode :: Text
sqrtCode = [__i|import "math"
                func test() {
                    result := math.Sqrt(16.0)
                    _ = result
                }|]
