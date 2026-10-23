{-# LANGUAGE OverloadedStrings #-}

module Spec.Tests.Java.LSP.Diagnostics (tests) where

import Data.Text (Text)
import Language.LSP.Protocol.Types
import Spec.Tests.Java.Common
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.Types


-- | Also the test that the invisible project gets built: without one, JDT LS reports only
-- syntax errors and these type errors go unmentioned.
tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Diagnostics" $ do
  -- Cell line 1, which the projection moves to line 2. No apostrophe in the name: Sandwich
  -- names the directory after it, and jdtls re-encodes %27, which breaks the proxy's reverse
  -- URI lookup and leaves diagnostics in shadow coordinates.
  testDiagnosticsLabel' projectTimeout "reports a cell diagnostic at the notebook line" lsName "main.ipynb" LanguageKind_Java notebookCode $ \diagnostics ->
    assertDiagnosticRanges diagnostics [(Range (Position 1 10) (Position 1 22), Just (InR "16777233"))]

  testDiagnosticsLabel' projectTimeout "reports a call to a method that doesn't exist" lsName "test.java" LanguageKind_Java undefinedMethodCode $ \diagnostics ->
    assertDiagnosticRanges diagnostics [(Range (Position 2 4) (Position 2 19), Just (InR "67108964"))]

  testDiagnosticsLabel' projectTimeout "reports a type error" lsName "test.java" LanguageKind_Java typeErrorCode $ \diagnostics ->
    assertDiagnosticRanges diagnostics [(Range (Position 1 10) (Position 1 22), Just (InR "16777233"))]

notebookCode :: Text
notebookCode = "int ok = 1;\nint bad = \"not an int\";"

undefinedMethodCode :: Text
undefinedMethodCode = "class Example {\n  void run() {\n    undefinedMethod();\n  }\n}\n"

typeErrorCode :: Text
typeErrorCode = "class Example {\n  int x = \"not an int\";\n}\n"
