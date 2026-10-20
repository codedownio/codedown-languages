module Spec.Tests.Haskell.Definition (tests) where

import Data.String.Interpolate
import Data.Text as T
import Language.LSP.Protocol.Types
import Language.LSP.Test hiding (message)
import qualified Language.LSP.Test.Helpers as Helpers
import Spec.Tests.Haskell.Common
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Definition" $ do
  it "finds foo (.hs)" $ doSession' "Test.hs" lsName definitionCodeRegular $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName "haskell"
    defs <- getDefinitions ident (Position 2 11)
    definitionRanges ident defs `shouldBe` [Range (Position 0 0) (Position 0 3)]

  it "finds foo (.ipynb)" $ do
    -- Core transforms the request but not the response, so the Location names the shadow
    -- document. typeDefinition and implementation share that transform and are wrong too.
    _ <- pending
    doNotebookSession lsName definitionCode $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName "haskell"
      defs <- getDefinitions ident (Position 1 10)
      definitionRanges ident defs `shouldBe` [Range (Position 0 0) (Position 0 3)]

definitionCode :: Text
definitionCode = [__i|foo = "hello"
                      putStrLn foo|]

definitionCodeRegular :: Text
definitionCodeRegular = [__i|foo = "hello"
                             main = do
                               putStrLn foo|]
