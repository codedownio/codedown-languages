module Spec.Tests.Haskell.RawFiles (tests) where

import Data.String.Interpolate
import Data.Text as T
import Language.LSP.Protocol.Types
import Language.LSP.Test hiding (message)
import qualified Language.LSP.Test.Helpers as Helpers
import Spec.Tests.Haskell.Common
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.Types


-- | The HLS config used to claim no file extensions unless it was the raw server, so a plain
-- .hs file in a sandbox reached no language server at all while notebooks worked.
tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Raw files" $ do
  it "serves a plain .hs file" $ doSession' "main.hs" lsName rawCode $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName "haskell"

    hover <- getHoverOrException ident (Position 0 1)
    allHoverText hover `textShouldContain` [i|foo|]

rawCode :: Text
rawCode = [__i|foo = "hello"
               main = putStrLn foo|]
