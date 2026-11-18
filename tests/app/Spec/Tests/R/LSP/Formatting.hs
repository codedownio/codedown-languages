module Spec.Tests.R.LSP.Formatting (tests) where

import Language.LSP.Protocol.Types
import Spec.Tests.R.Common
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = itFormatsAs lsName "test.R" LanguageKind_R
  "f <- function(a,b){\na+b\n}\n"
  "f <- function(a, b) {\n  a + b\n}\n"
