module Spec.Tests.Cpp.LSP.Diagnostics (tests) where

import Data.String.Interpolate
import Language.LSP.Protocol.Types
import Spec.Tests.Cpp.Common
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = testDiagnostics'' "simple" lsName "test.cpp" LanguageKind_CPP
  [__i|int main() {
         undefined_function();
         return 0;
       }|] [] $ \diags -> do
      info [i|Got diags: #{diags}|]
      info [i|Got ranges: #{getDiagnosticRanges' diags}|]
      getDiagnosticRanges' diags `shouldBe` [(Range (Position 1 2) (Position 1 20), Just (InR "undeclared_var_use"), "Use of undeclared identifier 'undefined_function'")]
