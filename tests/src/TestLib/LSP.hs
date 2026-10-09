{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-} -- For PlainString, CodeString, etc.
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module TestLib.LSP (
  doNotebookSession
  , doSession'
  , doSession''

  , Helpers.getDiagnosticRanges
  , Helpers.getDiagnosticRanges'
  , assertDiagnosticRanges
  , assertDiagnosticRanges'
  , assertDiagnosticRangesFromSource'
  , testDiagnostics
  , testDiagnosticsLabel
  , testDiagnosticsLabelDesired
  , testDiagnostics''

  , itHasHoverSatisfying

  , module TestLib.LSP.Formatting

  , Helpers.getHoverOrException
  , Helpers.allHoverText
  , Helpers.containsAll

  , LspContext
  ) where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.List as L
import Data.String.Interpolate
import qualified Data.Text as T hiding (filter)
import Data.Text hiding (filter, show)
import GHC.Int
import GHC.Stack
import Language.LSP.Protocol.Types
import Language.LSP.Test
import Language.LSP.Test.Helpers (LspContext, LspSessionOptions(..), withLspSession)
import qualified Language.LSP.Test.Helpers as Helpers
import Test.Sandwich as Sandwich
import TestLib.LSP.Formatting
import TestLib.LSP.Session
import TestLib.Types


doNotebookSession :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Text -> Text -> (Helpers.LspSessionInfo -> Session (ExampleT ctx m) a) -> ExampleT ctx m a
doNotebookSession = doSession' "main.ipynb"

doSession' :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Text -> Text -> Text -> (Helpers.LspSessionInfo -> Session (ExampleT ctx m) a) -> ExampleT ctx m a
doSession' filename lsName codeToUse cb = doSession'' filename lsName codeToUse [] cb

doSession'' :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Text -> Text -> Text -> [(FilePath, B.ByteString)] -> (Helpers.LspSessionInfo -> Session (ExampleT ctx m) a) -> ExampleT ctx m a
doSession'' filename lsName codeToUse extraFiles cb = do
  lspSessionOptions <- lspSessionOptionsFor lsName (T.unpack filename) LanguageKind_Python codeToUse
  withLspSession (lspSessionOptions { lspSessionOptionsExtraFiles = extraFiles }) cb

testDiagnostics :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Text -> FilePath -> LanguageKind -> Text -> ([Diagnostic] -> ExampleT ctx m ()) -> SpecFree ctx m ()
testDiagnostics name filename languageKind code = testDiagnostics' name filename languageKind code []

testDiagnostics' :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Text -> FilePath -> LanguageKind -> Text -> [(FilePath, B.ByteString)] -> ([Diagnostic] -> ExampleT ctx m ()) -> SpecFree ctx m ()
testDiagnostics' name filename languageKind codeToTest = testDiagnostics'' [i|#{name}, #{filename} with #{show codeToTest} (diagnostics)|] name filename languageKind codeToTest

testDiagnosticsLabel :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => String -> Text -> FilePath -> LanguageKind -> Text -> ([Diagnostic] -> ExampleT ctx m ()) -> SpecFree ctx m ()
testDiagnosticsLabel label name filename languageKind codeToTest = testDiagnostics'' label name filename languageKind codeToTest []

testDiagnosticsLabelDesired :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => String -> Text -> FilePath -> LanguageKind -> Text -> ([Diagnostic] -> Bool) -> SpecFree ctx m ()
testDiagnosticsLabelDesired label name filename languageKind code cb = it label $ do
  lspSessionOptions <- lspSessionOptionsFor name filename languageKind code

  Helpers.testDiagnostics lspSessionOptions languageKind $ \diags ->
    if | cb diags -> return ()
       | otherwise -> expectationFailure [i|Got unexpected diagnostics: #{diags}|]

testDiagnostics'' :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => String -> Text -> FilePath -> LanguageKind -> Text -> [(FilePath, B.ByteString)] -> ([Diagnostic] -> ExampleT ctx m ()) -> SpecFree ctx m ()
testDiagnostics'' label name filename languageKind code extraFiles cb = it label $ do
  lspSessionOptions <- (\o -> o { lspSessionOptionsExtraFiles = extraFiles })
                         <$> lspSessionOptionsFor name filename languageKind code

  Helpers.testDiagnostics lspSessionOptions languageKind $ \diags -> do
    lift $ cb diags

itHasHoverSatisfying :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Text -> FilePath -> LanguageKind -> Text -> Position -> (Hover -> ExampleT ctx m ()) -> SpecFree ctx m ()
itHasHoverSatisfying name filename languageKind code pos cb = it [i|#{name}: #{show code} (hover)|] $ do
  lspSessionOptions <- lspSessionOptionsFor name filename languageKind code

  withLspSession lspSessionOptions $ \_ -> do
    ident <- openDoc filename languageKind
    getHover ident pos >>= \case
      Nothing -> expectationFailure [i|Expected a hover.|]
      Just x -> lift $ cb x

assertDiagnosticRanges :: (HasCallStack, MonadIO m) => [Diagnostic] -> [(Range, Maybe (Int32 |? Text))] -> ExampleT ctx m ()
assertDiagnosticRanges = assertDiagnosticRanges'' Helpers.getDiagnosticRanges

assertDiagnosticRanges' :: (HasCallStack, MonadIO m) => [Diagnostic] -> [(Range, Maybe (Int32 |? Text), Text)] -> m ()
assertDiagnosticRanges' = assertDiagnosticRanges'' Helpers.getDiagnosticRanges'

assertDiagnosticRanges'' :: (HasCallStack, MonadIO m, Eq a, ToJSON a) => ([Diagnostic] -> [a]) -> [Diagnostic] -> [a] -> m ()
assertDiagnosticRanges'' keyFn diagnostics desired = if
  | keyFn diagnostics == desired -> return ()
  | otherwise ->
      expectationFailure [__i|Got wrong diagnostics!

                              Expected: #{A.encode desired}

                              Found: #{A.encode $ keyFn diagnostics}
                             |]

assertDiagnosticRangesFromSource' :: (HasCallStack, MonadIO m) => Text -> [Diagnostic] -> [(Range, Maybe (Int32 |? Text), Text)] -> m ()
assertDiagnosticRangesFromSource' src diagnostics =
  assertDiagnosticRanges' (L.filter (\d -> _source d == Just src) diagnostics)
