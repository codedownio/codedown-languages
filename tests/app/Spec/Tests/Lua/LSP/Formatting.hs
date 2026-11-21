module Spec.Tests.Lua.LSP.Formatting (tests) where

import Control.Monad
import Data.String.Interpolate
import Data.Text (Text)
import Language.LSP.Protocol.Types
import Spec.Tests.Lua.Common
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Formatting" $ forM_ ["test.lua", "main.ipynb"] $ \filename ->
  itFormatsAsLabel [i|#{lsName} formats #{filename}|] lsName filename LanguageKind_Lua
    badlyFormattedCode formattedCode

badlyFormattedCode :: Text
badlyFormattedCode = "local    x=1+2\nprint(  x )\n"

formattedCode :: Text
formattedCode = "local x = 1 + 2\nprint(x)\n"
