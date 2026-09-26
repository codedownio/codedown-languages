{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Typescript (tests) where

import Data.String.Interpolate
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types

import qualified Spec.Tests.Typescript.Completion as Completion
import qualified Spec.Tests.Typescript.Hovers as Hovers


-- | The TypeScript kernel is tslab without --js: same binary, same package set and
-- node_modules as the JavaScript kernel, but cells are TypeScript and are type checked.
tests :: LanguageSpec
tests = describe "TypeScript" $ do
  testKernelSearchersNonempty "typescript"
  testHasExpectedFields "typescript"

  introduceNixEnvironment [kernelSpec] [] "TypeScript" $ introduceJupyterRunner $ do
    describe "Kernel" $ do
      testKernelStdout "typescript" [__i|const greet = (name: string): string => `hi ${name}`;
                                         console.log(greet("world"))|] "hi world\n"

      testKernelStdout "typescript" [__i|interface Point { x: number; y: number }
                                         const p: Point = { x: 1, y: 2 };
                                         console.log(p.x + p.y)|] "3\n"

      -- Generics and the environment's types, together.
      testKernelStdout "typescript" [__i|import * as d3 from "d3";
                                         const s: d3.ScaleLinear<number, number> = d3.scaleLinear().domain([0, 10]).range([0, 100]);
                                         console.log(s(5))|] "50\n"

      testKernelStdout "typescript" [__i|import * as ss from "simple-statistics";
                                         const xs: number[] = [1, 2, 3, 4];
                                         console.log(ss.mean(xs))|] "2.5\n"

    describe "LSP" $ do
      Completion.tests

      Hovers.tests

kernelSpec :: NixKernelSpec
kernelSpec = NixKernelSpec {
  nixKernelName = "typescript"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "TypeScript"
  , nixKernelPackages = map nameOnly ["d3", "@types/d3", "simple-statistics"]
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelExtraConfig = Just [
      "lsp.typescript-language-server.enable = true"
      ]
  }

main :: IO ()
main = jupyterMain tests
