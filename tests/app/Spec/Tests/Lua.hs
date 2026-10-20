{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Lua (tests) where

import Control.Lens
import Control.Monad
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import Language.LSP.Protocol.Lens hiding (hover, text)
import Language.LSP.Protocol.Types
import Language.LSP.Test
import qualified Language.LSP.Test.Helpers as Helpers
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits (waitUntil)
import TestLib.JupyterRunnerContext
import TestLib.JupyterTypes
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types

import qualified Spec.Tests.Lua.Display as Display
import qualified Spec.Tests.Lua.VariableInspector as VariableInspector


tests :: LanguageSpec
tests = describe "Lua" $ do
  testKernelSearchersNonempty "lua"
  testHasExpectedFields "lua"

  introduceNixEnvironment [kernelSpec] [] "Lua" $ introduceJupyterRunner $ do
    describe "Kernel tests" $ do
      -- A cell whose whole body parses inside print(...) is run that way, by
      -- xeus-lua's auto_print. So a one-expression cell gets an extra newline from
      -- the wrapping print, and a multi-statement cell doesn't. Both are pinned
      -- here: this is the kernel's user-visible output format, and a change to it
      -- would be a change to every Lua notebook.
      testKernelStdout "lua" [__i|local greeting = "hi"
                                  print(greeting)|] "hi \n"

      testKernelStdout "lua" [__i|print("hi")|] "hi \n\n"

      -- io.write publishes its argument verbatim; print routes through pprint,
      -- which separates values with a space.
      testKernelStdout "lua" [__i|for i = 1, 3 do io.write(i) end
                                  io.write("\\n")|] "123\n"

      testKernelStdout "lua" [__i|local t = {1, 2, 3}
                                  print(table.concat(t, ","))|] "1,2,3 \n"

      it "reports the Lua version the kernel embeds" $
        testKernelStdout'' "lua" [__i|print(_VERSION)|] $ \case
          Nothing -> expectationFailure [i|Got no stdout|]
          Just t -> t `textShouldContain` "Lua 5.4"

      it "keeps globals across cells" $
        testKernelStdout'' "lua" [__i|counter = 41
                                      @@@CELL@@@
                                      local shown = counter + 1
                                      print(shown)|] $ \case
          Nothing -> expectationFailure [i|Got no stdout|]
          Just t -> t `shouldBe` "42 \n"

      -- A failing cell has to come back as an error output, not a silently
      -- successful empty one.
      testKernelErrors "lua" [__i|error("boom")|] (`textShouldContain` "boom")

      testKernelErrors "lua" [__i|noSuchFunction()|]
        (`textShouldContain` "attempt to call a nil value")

    describe "Packages" $
      introduceNixEnvironment [kernelSpecWithPackages] [] "Lua with packages" $ introduceJupyterRunner $ do
        -- inspect is pure Lua (found via LUA_PATH) and luafilesystem is a C module
        -- (found via LUA_CPATH), so these cover both halves of the module search
        -- path the kernel wrapper sets.
        testKernelStdout "lua" [__i|local inspect = require("inspect")
                                    print(inspect({1, 2}))|] "{ 1, 2 } \n"

        testKernelStdout "lua" [__i|local lfs = require("lfs")
                                    print(type(lfs.currentdir))|] "function \n"

    Display.tests "lua"

    VariableInspector.tests "lua"

    describe "LSP" $ forM_ ["test.lua", "main.ipynb"] $ \filename -> describe filename $ do
      -- lua-language-server answers every request with a "Workspace loading: n / m"
      -- placeholder until it has finished indexing its standard-library metadata, so
      -- these retry rather than asserting on the first reply.
      it [i|#{lsName}: hovers a standard library function (#{filename})|] $
        doSession' (T.pack filename) lsName [i|print("hi")|] $ \(Helpers.LspSessionInfo {..}) -> do
          ident <- openDoc lspSessionInfoFileName LanguageKind_Lua

          waitUntil 60 $ do
            hover <- Helpers.getHoverOrException ident (Position 0 2)
            Helpers.allHoverText hover `textShouldContain` "function print"

      it [i|#{lsName}: "str" includes "string" completion (#{filename})|] $
        doSession' (T.pack filename) lsName [i|local x = str|] $ \(Helpers.LspSessionInfo {..}) -> do
          ident <- openDoc lspSessionInfoFileName LanguageKind_Lua

          waitUntil 60 $ do
            completions <- getCompletions ident (Position 0 13)
            let labels = fmap (^. label) completions
            labels `shouldContain` ["string"]

      testDiagnosticsLabel [i|#{lsName}: Undefined global (#{filename})|] lsName filename LanguageKind_Lua
        undefinedGlobalCode $ \diagnostics ->
          assertDiagnosticRanges diagnostics [
            (Range (Position 0 10) (Position 0 29), Just (InR "undefined-global"))
            , (Range (Position 0 6) (Position 0 7), Just (InR "unused-local"))
            ]

      -- xeus-lua puts `ilua` and `pprint` in the global table before any cell runs,
      -- so the server's luarc lists them as known globals. Without that, every
      -- notebook that uses the kernel's own API is full of false positives.
      --
      -- The document ends with a genuinely undefined name on purpose: the diagnostics
      -- helper only fires on a non-empty publish, so "ilua is fine" has to be asserted
      -- as "these are the only diagnostics", not as "there are none".
      testDiagnosticsLabel [i|#{lsName}: the kernel's globals are not undefined (#{filename})|] lsName filename LanguageKind_Lua
        kernelGlobalsCode $ \diagnostics ->
          assertDiagnosticRanges diagnostics [
            (Range (Position 2 10) (Position 2 29), Just (InR "undefined-global"))
            , (Range (Position 2 6) (Position 2 7), Just (InR "unused-local"))
            ]

      itFormatsAsLabel [i|#{lsName} formats #{filename}|] lsName filename LanguageKind_Lua
        badlyFormattedCode formattedCode

lsName :: Text
lsName = "lua-language-server"

kernelSpec :: NixKernelSpec
kernelSpec = NixKernelSpec {
  nixKernelName = "lua"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "Lua"
  , nixKernelPackages = []
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelExtraConfig = Just ["lsp.lua-language-server.enable = true"]
  }

kernelSpecWithPackages :: NixKernelSpec
kernelSpecWithPackages = kernelSpec {
  nixKernelDisplayName = Just "Lua with packages"
  , nixKernelPackages = [nameOnly "inspect", nameOnly "luafilesystem"]
  }

undefinedGlobalCode :: Text
undefinedGlobalCode = [__i|local y = someUndefinedGlobal|]

kernelGlobalsCode :: Text
kernelGlobalsCode = [__i|ilua.config.printer = "pprint"
                         pprint({1, 2, 3})
                         local y = someUndefinedGlobal|]

badlyFormattedCode :: Text
badlyFormattedCode = "local    x=1+2\nprint(  x )\n"

formattedCode :: Text
formattedCode = "local x = 1 + 2\nprint(x)\n"

main :: IO ()
main = jupyterMain tests
