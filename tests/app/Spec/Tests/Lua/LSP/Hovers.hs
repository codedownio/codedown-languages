module Spec.Tests.Lua.LSP.Hovers (tests) where

import Control.Monad
import Data.String.Interpolate
import qualified Data.Text as T
import Language.LSP.Protocol.Types
import Language.LSP.Test
import qualified Language.LSP.Test.Helpers as Helpers
import Spec.Tests.Lua.Common
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits (waitUntil)
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Hovers" $ forM_ ["test.lua", "main.ipynb"] $ \filename ->
  it [i|#{lsName}: hovers a standard library function (#{filename})|] $
    doSession' (T.pack filename) lsName [i|print("hi")|] $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName LanguageKind_Lua

      waitUntil 60 $ do
        hover <- Helpers.getHoverOrException ident (Position 0 2)
        Helpers.allHoverText hover `textShouldContain` "function print"
