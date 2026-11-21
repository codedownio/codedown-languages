module Spec.Tests.Lua.LSP.Diagnostics (tests) where

import Control.Monad
import Data.String.Interpolate
import Data.Text (Text)
import Language.LSP.Protocol.Types
import Spec.Tests.Lua.Common
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Diagnostics" $ forM_ ["test.lua", "main.ipynb"] $ \filename -> do
  testDiagnosticsLabel [i|#{lsName}: Undefined global (#{filename})|] lsName filename LanguageKind_Lua
    undefinedGlobalCode $ \diagnostics ->
      assertDiagnosticRanges diagnostics [
        (Range (Position 0 10) (Position 0 29), Just (InR "undefined-global"))
        , (Range (Position 0 6) (Position 0 7), Just (InR "unused-local"))
        ]

  testDiagnosticsLabel [i|#{lsName}: the kernel's globals are not undefined (#{filename})|] lsName filename LanguageKind_Lua
    kernelGlobalsCode $ \diagnostics ->
      assertDiagnosticRanges diagnostics [
        (Range (Position 2 10) (Position 2 29), Just (InR "undefined-global"))
        , (Range (Position 2 6) (Position 2 7), Just (InR "unused-local"))
        ]

undefinedGlobalCode :: Text
undefinedGlobalCode = [__i|local y = someUndefinedGlobal|]

kernelGlobalsCode :: Text
kernelGlobalsCode = [__i|ilua.config.printer = "pprint"
                         pprint({1, 2, 3})
                         local y = someUndefinedGlobal|]
