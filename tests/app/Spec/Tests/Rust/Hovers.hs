
module Spec.Tests.Rust.Hovers where

import Control.Monad
import Data.String.Interpolate
import Language.LSP.Protocol.Types
import Language.LSP.Test
import Test.Sandwich as Sandwich
import Test.Sandwich.Contexts.Waits (waitUntil)
import TestLib.LSP


hoverTests :: (LspContext context m) => SpecFree context m ()
hoverTests = describe "Hovers" $ do
  forM_ ["main.ipynb", "test.rs"] $ \doc -> do
    it [i|hovers println! (#{doc})|] $ doSession' doc "rust-analyzer" [i|println!("hi")|] $ \filename -> do
      ident <- openDoc filename "haskell"

      waitUntil 60 $ do
        hover <- getHoverOrException ident (Position 0 1)
        allHoverText hover `textShouldContain` [i|Prints to the standard output|]
