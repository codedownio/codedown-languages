
module Spec.Tests.Haskell.CodeActions where

import Control.Lens
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.String.Interpolate
import Data.Text
import Language.LSP.Test hiding (message)
import Language.LSP.Types
import Language.LSP.Types.Lens hiding (actions)
import Spec.Tests.Haskell.Common
import Test.Sandwich as Sandwich
import TestLib.LSP


codeActionsTests :: (
  Sandwich.HasLabel context "nixEnvironment" FilePath, HasBaseContext context, MonadBaseControl IO m, MonadUnliftIO m, MonadThrow m
  ) => SpecFree context m ()
codeActionsTests = describe "Code actions" $ do
  it "gets no code actions for putStrLn" $ doNotebookSession lsName codeActionsCode $ \filename -> do
    ident <- openDoc filename "haskell"
    actions <- getCodeActions ident (Range (Position 1 0) (Position 1 8))
    actions `shouldBe` []

  it "gets code actions for foo" $ doNotebookSession lsName codeActionsCode $ \filename -> do
    ident <- openDoc filename "haskell"
    actions <- getCodeActions ident (Range (Position 0 0) (Position 0 3))
    fmap getTitle actions `shouldBe` ["Unfold foo"
                                     , "Unfold foo in current file"
                                     , "Fold foo"
                                     , "Fold foo in current file"
                                     ]

getTitle :: (HasTitle a Text, HasTitle b Text) => (a |? b) -> Text
getTitle (InL x) = x ^. title
getTitle (InR x) = x ^. title

codeActionsCode :: Text
codeActionsCode = [__i|foo = "hello"
                       putStrLn foo|]
