{-# LANGUAGE OverloadedStrings #-}

module Spec.Tests.Javascript.Packages (tests) where

import Data.String.Interpolate
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext


-- | The environment's node_modules is built from the closure of the selected packages, and
-- reaches the kernel through NODE_PATH -- so a selected package resolves from any working
-- directory, and one that wasn't selected must not resolve at all.
tests :: (HasJupyterRunnerContext context, JupyterRunnerMonad m) => SpecFree context m ()
tests = describe "Packages" $ do
  testKernelStdout "javascript" [__i|import * as d3 from "d3";
                                     console.log(typeof d3.scaleLinear)|] "function\n"

  testKernelStdout "javascript" [__i|import * as ss from "simple-statistics";
                                     console.log(ss.mean([1, 2, 3, 4]))|] "2.5\n"

  -- lodash is in the curated set but not in this environment's selection. The module name is
  -- held in a variable so this is a resolution failure at run time rather than a type error,
  -- which would fail the cell before it ran.
  testKernelStdout "javascript" [__i|const name = "lodash";
                                     try {
                                       require(name);
                                       console.log("resolved");
                                     } catch (e) {
                                       console.log("not available");
                                     }|] "not available\n"
