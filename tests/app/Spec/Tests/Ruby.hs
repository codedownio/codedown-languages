{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Spec.Tests.Ruby (tests) where

import Control.Lens
import Data.String.Interpolate
import Data.Text as T
import Language.LSP.Types
import Language.LSP.Types.Lens hiding (hover, text)
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers


tests :: TopSpec
tests = describe "Ruby" $ do
  kernelTests "ruby"
  kernelTests "ruby_2_7"
  kernelTests "ruby_3_0"
  kernelTests "ruby_3_1"


kernelTests :: Text -> TopSpec
kernelTests lang = do
  describe (T.unpack lang) $ introduceNixEnvironment [kernelSpec lang] [] [i|Ruby (#{lang})|] $ introduceJupyterRunner $ do
    testKernelSearchers lang

    testKernelStdout lang [__i|puts "hi"|] "hi\n"

    itHasHoverSatisfying "solargraph" "test.rb" [__i|puts "hi"|] (Position 0 2) $ \hover -> do
      let HoverContents (MarkupContent MkMarkdown text) = hover ^. contents
      text `textShouldContain` "Kernel#puts"
      text `textShouldContain` "$stdout.puts(obj"
      text `textShouldContain` "Returns:"

kernelSpec :: Text -> NixKernelSpec
kernelSpec lang = NixKernelSpec {
  nixKernelName = lang
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just [i|Ruby (#{lang})|]
  , nixKernelPackages = []
  , nixKernelLanguageServers = [nameOnly "solargraph"]
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Nothing
  }

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
