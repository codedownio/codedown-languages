
module Spec.Tests.Haskell.CodeActions where

import Control.Lens
import Data.String.Interpolate
import Data.Text
import Language.LSP.Test hiding (message)
import Language.LSP.Types
import Language.LSP.Types.Lens hiding (actions)
import Spec.Tests.Haskell.Common
import Test.Sandwich as Sandwich
import TestLib.LSP


codeActionsTests :: (LspContext context m) => SpecFree context m ()
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
