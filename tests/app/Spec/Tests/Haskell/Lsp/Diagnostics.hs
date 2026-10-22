{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

module Spec.Tests.Haskell.Lsp.Diagnostics (
  tests
  , main
  ) where

import Control.Lens ((^.))
import Data.String.Interpolate
import Language.LSP.Protocol.Lens hiding (diagnostics)
import Language.LSP.Protocol.Types
import Spec.Tests.Haskell.Common
import Spec.Tests.Haskell.GhcPackage
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.Types


tests :: (
  LspContext context m, HasNixEnvironment context, HasGhcPackage context
  ) => SpecFree context m ()
tests = describe "Diagnostics" $ do
  describe "Foo.hs" $ do
    testDiagnosticsLabelDesired "Out of scope variable" lsName "Foo.hs" LanguageKind_Haskell
      [__i|module Foo where
           foo = bar
          |]
      ((== [(Range (Position 1 6) (Position 1 9), Just (InR "GHC-88464"))]) . getDiagnosticRanges)

    skippedOnGhc910 $
      testDiagnosticsLabelDesired "Eta reduce" lsName "Foo.hs" LanguageKind_Haskell etaExpandCode
        ((== [(Range (Position 6 0) (Position 6 14), Just (InR "refact:Eta reduce"))]) . getDiagnosticRanges)

  describe "main.ipynb" $ do
    testDiagnosticsLabelDesired "Top-level putStrLn" lsName "main.ipynb" LanguageKind_Haskell
      [__i|-- A comment
           foo = bar

           putStrLn "HI"
          |]
      ((== [(Range (Position 1 6) (Position 1 9), Just (InR "GHC-88464"))]) . getDiagnosticRanges)

    testDiagnosticsLabel "Top-level putStrLn with diagnostic" lsName "main.ipynb" LanguageKind_Haskell
      [__i|-- Some comment
           import Data.ByteString.Lazy.Char8 as BL
           foo = bar

           putStrLn "HI"
          |] $ \diagnostics -> case [(x ^. range, x ^. message) | x <- diagnostics] of
        [(Range (Position 4 0) (Position 4 8), x)] | containsAll x ["Ambiguous occurrence", "putStrLn"] -> return ()
        xs -> expectationFailure [i|Unexpected diagnostics: #{xs}|]

    testDiagnosticsLabelDesired "Reordering" lsName "main.ipynb" LanguageKind_Haskell
      [__i|import Data.Aeson.TH
           {-\# LANGUAGE TemplateHaskell \#-}
           foo = bar -- This should be the only diagnostic we get
           data Foo = Bar | Baz
           $(deriveJSON defaultOptions ''Foo)|]
      ((== [(Range (Position 2 6) (Position 2 9), Just (InR "GHC-88464"))]) . getDiagnosticRanges)

    testDiagnosticsLabelDesired "Complicated reordering" lsName "main.ipynb" LanguageKind_Haskell
      [__i|import Data.Aeson as A
           import Data.Aeson.TH
           :set -XTemplateHaskell
           foo = bar -- This should be the only diagnostic we get
           data Foo = Bar | Baz
           $(deriveJSON defaultOptions ''Foo)
           import Data.ByteString.Lazy.Char8 as BL
           Prelude.putStrLn $ BL.unpack $ A.encode Bar|]
      ((== [(Range (Position 3 6) (Position 3 9), Just (InR "GHC-88464"))]) . getDiagnosticRanges)


main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions $
  introduceNixEnvironment [kernelSpec ghcPkg] [] "Haskell" $
    introduceJustBubblewrap $
      introduceGhcPackage ghcPkg $
        tests
  where
    ghcPkg = "ghc92"
