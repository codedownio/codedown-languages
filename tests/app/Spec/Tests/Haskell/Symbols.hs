
module Spec.Tests.Haskell.Symbols where

import Control.Lens ((^.))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control (MonadBaseControl)
import Language.LSP.Test hiding (message)
import Language.LSP.Types.Lens hiding (actions)
import Spec.Tests.Haskell.Common
import Spec.Tests.Haskell.DocumentHighlight (documentHighlightCode)
import Test.Sandwich as Sandwich
import TestLib.LSP


symbolsTests :: (
  Sandwich.HasLabel context "nixEnvironment" FilePath, HasBaseContext context, MonadBaseControl IO m, MonadUnliftIO m, MonadThrow m
  ) => SpecFree context m ()
symbolsTests = describe "Symbols" $ do
  it "symbols" $ doNotebookSession lsName documentHighlightCode $ \filename -> do
    ident <- openDoc filename "haskell"
    Left documentSymbols <- getDocumentSymbols ident
    fmap (^. name) documentSymbols `shouldBe` ["foo"]
