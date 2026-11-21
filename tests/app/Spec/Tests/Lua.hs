{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Lua (tests) where

import Data.String.Interpolate
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types

import qualified Spec.Tests.Lua.Display as Display
import qualified Spec.Tests.Lua.LSP as LSP
import qualified Spec.Tests.Lua.VariableInspector as VariableInspector


tests :: LanguageSpec
tests = describe "Lua" $ do
  testKernelSearchersNonempty "lua"
  testHasExpectedFields "lua"

  introduceNixEnvironment [kernelSpec] [] "Lua" $ introduceJupyterRunner $ do
    describe "Kernel tests" $ do
      testKernelStdout "lua" [__i|local greeting = "hi"
                                  print(greeting)|] "hi \n"

      testKernelStdout "lua" [__i|print("hi")|] "hi \n\n"

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

      testKernelErrorContaining "lua" [__i|error("boom")|] "boom"

      testKernelErrorContaining "lua" [__i|noSuchFunction()|] "attempt to call a nil value"

    describe "Packages" $
      introduceNixEnvironment [kernelSpecWithPackages] [] "Lua with packages" $ introduceJupyterRunner $ do
        testKernelStdout "lua" [__i|local inspect = require("inspect")
                                    print(inspect({1, 2}))|] "{ 1, 2 } \n"

        testKernelStdout "lua" [__i|local lfs = require("lfs")
                                    print(type(lfs.currentdir))|] "function \n"

    Display.tests "lua"

    VariableInspector.tests "lua"

    LSP.tests

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

main :: IO ()
main = jupyterMain tests
