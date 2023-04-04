{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Julia (tests) where

import Data.String.Interpolate
import Data.Text as T
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers


tests :: TopSpec
tests = do
  juliaTests "julia"
  juliaTests "julia16"
  juliaTests "julia18"

juliaTests :: Text -> TopSpec
juliaTests lang = describe [i|Julia (#{lang})|] $ introduceNixEnvironment [kernelSpec lang] [] [i|Julia (#{lang})|] $ introduceJupyterRunner $ do
  testKernelSearchers lang
  testKernelStdout lang [i|println("hi")|] "hi\n"

  testDiagnostics "LanguageServer" "test.jl" [i|printlnz("HI")|] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics []
    -- assertDiagnosticRanges diagnostics [(Range (Position 3 16) (Position 3 19), Just (InR "UndeclaredName"))]

kernelSpec :: Text -> NixKernelSpec
kernelSpec lang = NixKernelSpec {
  nixKernelName = lang
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just [i|Julia (#{lang})|]
  , nixKernelPackages = []
  , nixKernelLanguageServers = [nameOnly "LanguageServer"]
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Nothing
  }

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
