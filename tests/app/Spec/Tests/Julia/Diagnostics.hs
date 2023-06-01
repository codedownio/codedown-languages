{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Spec.Tests.Julia.Diagnostics where

import Data.String.Interpolate
import Data.Text as T
import Language.LSP.Types
import Test.Sandwich as Sandwich
import TestLib.LSP


diagnosticsTests :: (LspContext context m) => Text -> SpecFree context m ()
diagnosticsTests lsName = describe "Diagnostics" $ do
  testDiagnostics'' "flags a simple missing reference" lsName "test.jl" (Just "julia") [i|printlnzzzz("HI")|] [] $ \diagnostics -> do
    assertDiagnosticRanges' diagnostics [(Range (Position 0 0) (Position 0 11), Nothing, "Missing reference: printlnzzzz")]

  testDiagnostics'' "finds symbols from JSON3 package" lsName "test.jl" (Just "julia")
    [__i|using JSON3
         tup = (:car,"Mercedes","S500",5,250.1)
         JSON3.write(tup)
         printlnzzzz("HI")
         |] [] $ \diagnostics -> do
    assertDiagnosticRanges' diagnostics [(Range (Position 3 0) (Position 3 11), Nothing, "Missing reference: printlnzzzz")]

  testDiagnostics'' "finds symbols from Roots package" lsName "test.jl" (Just "julia")
    [__i|using Roots
         f(x) = exp(x) - x^4
         find_zero(f, (8, 9), Bisection())
         printlnzzzz("HI")
         |] [] $ \diagnostics -> do
    assertDiagnosticRanges' diagnostics [(Range (Position 3 0) (Position 3 11), Nothing, "Missing reference: printlnzzzz")]

  -- This test is tricky because symbols like "plot" actually come from RecipesBase (or something), so
  -- it checks we're indexing such a transitive dependency.
  testDiagnostics'' "finds symbols from Plots package" lsName "test.jl" (Just "julia")
    [__i|using Plots
         xx = range(0, 10, length=100)
         y = sin.(xx)
         plot(xx, y)
         plot!(xx, y)
         printlnzzzz("HI")
         |] [] $ \diagnostics -> do
    assertDiagnosticRanges' diagnostics [(Range (Position 5 0) (Position 5 11), Nothing, "Missing reference: printlnzzzz")]
