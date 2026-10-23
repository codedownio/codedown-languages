module Spec.Tests.Javascript.LSP.Formatting (tests) where

import Language.LSP.Protocol.Types
import Spec.Tests.Javascript.Common
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = itFormatsAs lsName "test.js" LanguageKind_JavaScript
  "const x   =   1+2\nconsole.log( x )\n"
  "const x = 1 + 2\nconsole.log(x)\n"
