module Spec.Tests.Go.LSP.Diagnostics (tests) where

import Data.String.Interpolate
import Data.Text (Text)
import Language.LSP.Protocol.Types
import Spec.Tests.Go.Common
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = testDiagnosticsLabel "gopls: Undeclared name" lsName "test.go" LanguageKind_Go printUnknownCode $ \diagnostics ->
  assertDiagnosticRanges diagnostics [(Range (Position 3 12) (Position 3 15), Just (InR "UndeclaredName"))]

printUnknownCode :: Text
printUnknownCode = [__i|package main
                        import ("fmt")
                        func main() {
                        fmt.Println(foo)
                        }|]
