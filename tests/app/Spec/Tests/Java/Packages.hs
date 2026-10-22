{-# LANGUAGE OverloadedStrings #-}

module Spec.Tests.Java.Packages (tests) where

import Data.String.Interpolate
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext


-- | A selected library resolves from any working directory; an unselected one must not.
tests :: (HasJupyterRunnerContext context, JupyterRunnerMonad m) => SpecFree context m ()
tests = describe "Packages" $ do
  testKernelStdout "java" [__i|import org.apache.commons.lang3.StringUtils;
                               System.out.println(StringUtils.reverse("abc"));|] "cba\n"

  testKernelStdout "java" [__i|import com.google.common.collect.ImmutableList;
                               System.out.println(ImmutableList.of(1, 2, 3).reverse());|] "[3, 2, 1]\n"

  -- jsoup is curated but not selected. Named as a string so this fails at run time rather
  -- than compile time.
  testKernelStdout "java" [__i|try {
                                 Class.forName("org.jsoup.Jsoup");
                                 System.out.println("resolved");
                               } catch (ClassNotFoundException e) {
                                 System.out.println("not available");
                               }|] "not available\n"
