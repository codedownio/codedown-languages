
module Spec.Tests.Haskell.DocumentHighlight where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.String.Interpolate
import Data.Text as T
import Language.LSP.Test hiding (message)
import Language.LSP.Types
import Spec.Tests.Haskell.Common
import Test.Sandwich as Sandwich


documentHighlightTests :: (
  Sandwich.HasLabel context "nixEnvironment" FilePath, HasBaseContext context, MonadBaseControl IO m, MonadUnliftIO m, MonadThrow m
  ) => SpecFree context m ()
documentHighlightTests = describe "Document highlight" $ do
  it "highlights foo" $ doNotebookSession documentHighlightCode $ \filename -> do
    ident <- openDoc filename "haskell"
    getHighlights ident (Position 0 1) >>= (`shouldBe` List documentHighlightResults)

documentHighlightCode :: Text
documentHighlightCode = [__i|foo = "hello"
                             putStrLn foo|]

documentHighlightResults :: [DocumentHighlight]
documentHighlightResults = [
  DocumentHighlight (Range (Position 0 0) (Position 0 3)) (Just HkWrite)
  , DocumentHighlight (Range (Position 1 9) (Position 1 12)) (Just HkRead)
  ]
