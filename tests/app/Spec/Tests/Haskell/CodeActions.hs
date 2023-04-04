
module Spec.Tests.Haskell.CodeActions where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control (MonadBaseControl)
import Language.LSP.Test hiding (message)
import Language.LSP.Types
import Spec.Tests.Haskell.Common
import Spec.Tests.Haskell.DocumentHighlight (documentHighlightCode)
import Test.Sandwich as Sandwich


codeActionsTests :: (
  Sandwich.HasLabel context "nixEnvironment" FilePath, HasBaseContext context, MonadBaseControl IO m, MonadUnliftIO m, MonadThrow m
  ) => SpecFree context m ()
codeActionsTests = describe "Code actions" $ do
  it "gets no code actions for putStrLn" $ doNotebookSession documentHighlightCode $ \filename -> do
    ident <- openDoc filename "haskell"
    actions <- getCodeActions ident (Range (Position 1 0) (Position 1 8))
    actions `shouldBe` []

  it "gets code actions for foo" $ doNotebookSession documentHighlightCode $ \filename -> do
    ident <- openDoc filename "haskell"
    actions <- getCodeActions ident (Range (Position 1 0) (Position 1 8))
    actions `shouldBe` []
