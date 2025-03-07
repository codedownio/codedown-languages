{-# LANGUAGE OverloadedLabels #-}

module Spec.Tests.Rust.Changes (tests) where

import Control.Lens
import Data.Function
import qualified Data.List as L
import Data.Row.Records
import Data.String.Interpolate
import qualified Language.LSP.Protocol.Lens as LSP
import Language.LSP.Protocol.Types
import Language.LSP.Test
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits (waitUntil)
import TestLib.LSP


tests :: (LspContext context m) => SpecFree context m ()
tests = describe "Changes" $ do
  it [i|Simple change|] $ doSession' "main.ipynb" "rust-analyzer" [i|println("hi");|] $ \filename -> do
    ident <- openDoc filename "haskell"

    waitUntil 120.0 $ do
      waitForDiagnostics >>= \diags -> do
        assertDiagnosticRanges' (L.sortBy (compare `on` (^. LSP.message)) diags) [
          (Range (Position 0 0) (Position 0 7), Just (InR "E0423"), "expected function, found macro `println`\nnot a function")
          , (Range (Position 0 7) (Position 0 7), Just (InR "E0423"), "use `!` to invoke the macro: `!`")
          ]

    changeDoc ident [TextDocumentContentChangeEvent $ InL (#range .== (Range (p 0 7) (p 0 7)) .+ #rangeLength .== Nothing .+ #text .== "z")]

    waitUntil 120.0 $ do
      waitForDiagnostics >>= \diags -> do
        assertDiagnosticRanges' (L.sortBy (compare `on` (^. LSP.message)) diags) [
          (Range (Position 0 0) (Position 0 8), Just (InR "E0425"), "cannot find function `printlnz` in this scope\nnot found in this scope")
          ]


p :: Int -> Int -> Position
p l c = Position (fromIntegral l) (fromIntegral c)
