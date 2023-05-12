{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Spec.Tests.Julia.Diagnostics where

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.String.Interpolate
import Data.Text as T
import Language.LSP.Types
import Test.Sandwich as Sandwich
import TestLib.LSP


diagnosticsTests :: (
  Sandwich.HasLabel context "nixEnvironment" FilePath, HasBaseContext context, MonadBaseControl IO m, MonadUnliftIO m, MonadThrow m, MonadCatch m
  ) => Text -> SpecFree context m ()
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

  testDiagnostics'' "finds symbols from Plots package" lsName "test.jl" (Just "julia")
    [__i|using Plots
         xx = range(0, 10, length=100)
         y = sin.(xx)
         plot(xx, y)
         printlnzzzz("HI")
         |] [] $ \diagnostics -> do
    assertDiagnosticRanges' diagnostics [(Range (Position 4 0) (Position 4 11), Nothing, "Missing reference: printlnzzzz")]
