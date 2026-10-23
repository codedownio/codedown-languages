module Spec.Tests.Typescript.LSP.Formatting (tests) where

import Language.LSP.Protocol.Types
import Spec.Tests.Typescript.Common
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = itFormatsAs lsName "test.ts" LanguageKind_TypeScript
  "const x : number  =   1+2\nconsole.log( x )\n"
  "const x: number = 1 + 2\nconsole.log(x)\n"
