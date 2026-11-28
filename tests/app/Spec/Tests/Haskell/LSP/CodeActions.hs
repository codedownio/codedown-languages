
module Spec.Tests.Haskell.LSP.CodeActions (tests) where

import Data.String.Interpolate
import Data.Text
import Language.LSP.Protocol.Types
import Language.LSP.Test hiding (message)
import qualified Language.LSP.Test.Helpers as Helpers
import Spec.Tests.Haskell.Common
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.Types
import UnliftIO.Timeout


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Code actions" $ do
  it "gets no code actions for putStrLn" $ doNotebookSession lsName codeActionsCode $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName "haskell"
    actions <- timeout 60_000_000 $ getCodeActions ident (Range (Position 1 0) (Position 1 8))
    actions `shouldBe` (Just [])

codeActionsCode :: Text
codeActionsCode = [__i|foo = "hello"
                       putStrLn foo|]
