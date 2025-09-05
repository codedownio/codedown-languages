{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Spec.Tests.Julia.Diagnostics (tests) where

import Data.String.Interpolate
import Data.Text as T
import Language.LSP.Protocol.Types
import Test.Sandwich as Sandwich
import TestLib.LSP


tests :: (LspContext context m) => Text -> Text -> SpecFree context m ()
tests juliaPackage lsName = describe "Diagnostics" $ do
  testDiagnostics'' "flags a simple missing reference" lsName "test.jl" (Just "julia") [i|printlnzzzz("HI")|] [] $ \diagnostics -> do
    assertDiagnosticRanges' diagnostics [(Range (Position 0 0) (Position 0 11), Nothing, "Missing reference: printlnzzzz")]

  testDiagnosticsLabelDesired "finds symbols from JSON3 package" lsName "test.jl" (Just "julia")
    [__i|using JSON3: write
         tup = (:car,"Mercedes","S500",5,250.1)
         write(tup)
         printlnzzzz("HI")
         |]
    ((== [(Range (Position 3 0) (Position 3 11), Nothing, "Missing reference: printlnzzzz")]) . getDiagnosticRanges')

  testDiagnosticsLabelDesired "finds symbols from Roots package" lsName "test.jl" (Just "julia")
    [__i|using Roots: Bisection, find_zero
         f(x) = exp(x) - x^4
         find_zero(f, (8, 9), Bisection())
         printlnzzzz("HI")
         |]
    ((== [(Range (Position 3 0) (Position 3 11), Nothing, "Missing reference: printlnzzzz")]) . getDiagnosticRanges')

  -- This test is tricky because symbols like "plot" actually come from RecipesBase (or something), so
  -- it checks we're indexing such a transitive dependency.
  testDiagnosticsLabelDesired "finds symbols from Plots package" lsName "test.jl" (Just "julia")
    [__i|using Plots: plot, plot!
         xx = range(0, 10, length=100)
         y = sin.(xx)
         plot(xx, y)
         plot!(xx, y)
         printlnzzzz("HI")
         |]
    ((== [(Range (Position 5 0) (Position 5 11), Nothing, "Missing reference: printlnzzzz")]) . getDiagnosticRanges')
