{-# LANGUAGE RankNTypes #-}

module Spec.Tests.Julia (tests) where

import Data.String.Interpolate
import Data.Text as T
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers


tests :: TopSpec
tests = do
  juliaTests "julia-stable-bin"
  juliaTests "julia-lts-bin"

juliaTests :: Text -> TopSpec
juliaTests lang = describe "Julia" $ introduceNixEnvironment [kernelSpec lang] [] [i|Julia (#{lang})|] $ introduceJupyterRunner $ do
  testKernelSearchers lang
  testKernelStdout lang [i|print("hi")|] "hi\n"

kernelSpec lang = NixKernelSpec {
  nixKernelName = lang
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just [i|Julia (#{lang})|]
  , nixKernelPackages = []
  , nixKernelLanguageServers = []
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Nothing
  }

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
