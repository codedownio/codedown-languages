module Spec.Tests.R.LSP.Diagnostics (tests) where

import Data.Text (Text)
import Language.LSP.Protocol.Types
import Spec.Tests.R.Common
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Diagnostics" $ do
  testDiagnosticsLabel "lints = assignment (test.R)" lsName "test.R" LanguageKind_R code $ \diagnostics ->
    assertDiagnosticRanges diagnostics [(Range (Position 0 2) (Position 0 3), Just (InR "assignment_linter"))]

  testDiagnosticsLabel "lints = assignment (main.ipynb)" lsName "main.ipynb" LanguageKind_R code $ \diagnostics ->
    assertDiagnosticRanges diagnostics [(Range (Position 0 2) (Position 0 3), Just (InR "assignment_linter"))]

code :: Text
code = "x = 1\n"
