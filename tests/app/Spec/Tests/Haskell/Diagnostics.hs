{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}

module Spec.Tests.Haskell.Diagnostics where

import Control.Lens ((^.))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.String.Interpolate
import Data.Text as T
import Language.LSP.Types
import Language.LSP.Types.Lens hiding (diagnostics)
import Test.Sandwich as Sandwich
import TestLib.LSP


diagnosticsTests :: (
  Sandwich.HasLabel context "nixEnvironment" FilePath, HasBaseContext context, MonadBaseControl IO m, MonadUnliftIO m, MonadThrow m
  ) => Text -> SpecFree context m ()
diagnosticsTests lsName = describe "Diagnostics" $ do
  testDiagnostics lsName "Foo.hs" [__i|module Foo where
                                       foo = bar
                                      |] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics [(Range (Position 1 6) (Position 1 9), Just (InR "-Wdeferred-out-of-scope-variables"))]

  testDiagnostics lsName "Foo.hs" etaExpandCode $ \diagnostics -> do
    assertDiagnosticRanges diagnostics [(Range (Position 6 0) (Position 6 14), Just (InR "refact:Eta reduce"))]

  testDiagnostics lsName "main.ipynb" [__i|-- Some comment
                                           foo = bar

                                           putStrLn "HI"
                                          |] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics [(Range (Position 1 6) (Position 1 9), Just (InR "-Wdeferred-out-of-scope-variables"))]

  testDiagnostics lsName "main.ipynb" [__i|import Data.Aeson.TH
                                           {-\# LANGUAGE TemplateHaskell \#-}
                                           foo = bar -- This should be the only diagnostic we get
                                           data Foo = Bar | Baz
                                           $(deriveJSON defaultOptions ''Foo)|] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics [(Range (Position 2 6) (Position 2 9), Just (InR "-Wdeferred-out-of-scope-variables"))]

  testDiagnostics lsName "main.ipynb" [__i|import Data.Aeson as A
                                           import Data.Aeson.TH
                                           :set -XTemplateHaskell
                                           foo = bar -- This should be the only diagnostic we get
                                           data Foo = Bar | Baz
                                           $(deriveJSON defaultOptions ''Foo)
                                           import Data.ByteString.Lazy.Char8 as BL
                                           Prelude.putStrLn $ BL.unpack $ A.encode Bar|] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics [(Range (Position 3 6) (Position 3 9), Just (InR "-Wdeferred-out-of-scope-variables"))]

  testDiagnostics lsName "main.ipynb" [__i|-- Some comment
                                           import Data.ByteString.Lazy.Char8 as BL
                                           foo = bar

                                           putStrLn "HI"
                                          |] $ \diagnostics -> case [(x ^. range, x ^. message) | x <- diagnostics] of
    [(Range (Position 4 0) (Position 4 8), x)] | containsAll x ["Ambiguous occurrence", "putStrLn"] -> return ()
    xs -> expectationFailure [i|Unexpected diagnostics: #{xs}|]


etaExpandCode :: Text
etaExpandCode = [__i|module Foo where

                     baz :: Int -> Int
                     baz x = x + 1

                     baz2 :: Int -> Int
                     baz2 x = baz x
                    |]
