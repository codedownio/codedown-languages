{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Spec.Tests.Haskell.Hover where

import Control.Lens ((^.))
import Data.String.Interpolate
import Data.Text as T
import Language.LSP.Test hiding (message)
import Language.LSP.Types
import Language.LSP.Types.Lens hiding (hover)
import Spec.Tests.Haskell.Common
import Test.Sandwich as Sandwich
import TestLib.LSP


hoverTests :: (LspContext context m) => SpecFree context m ()
hoverTests = describe "Hover" $ do
  it "hovers foo" $ doNotebookSession lsName hoverCode $ \filename -> do
    ident <- openDoc filename "haskell"
    hover <- getHoverOrException ident (Position 0 1)
    allHoverText hover `textShouldContain` [i|foo|]
    allHoverText hover `textShouldContain` [i|main.ipynb.hs:1:1|]

  it "hovers putStrLn" $ doNotebookSession lsName hoverCode $ \filename -> do
    ident <- openDoc filename "haskell"
    hover <- getHoverOrException ident (Position 1 1)
    (hover ^. range) `shouldBe` Just (Range (Position 1 0) (Position 1 8))
    let HoverContents (MarkupContent {..}) = hover ^. contents
    _value `textShouldContain` "putStrLn :: String -> IO ()"

hoverCode :: Text
hoverCode = [__i|foo = "hello"
                 putStrLn foo
                 import Data.Aeson|]
