module Spec.Tests.Julia.LSP.Formatting (tests) where

import Spec.Tests.Julia.Common
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.Types


-- Weaker than the other languages' formatting tests: JuliaFormatter's exact output isn't
-- pinned here, only that LanguageServer.jl actually reformats.
tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = itReformats lsName "test.jl" juliaLanguageKind
  "function foo(a,b)\nx=a+b\nreturn x\nend\n"
