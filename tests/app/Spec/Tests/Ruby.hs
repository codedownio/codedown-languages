{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Spec.Tests.Ruby (tests) where

import Control.Lens
import Data.Aeson as A
import Data.String.Interpolate
import Data.Text as T
import Language.LSP.Protocol.Lens hiding (hover, text)
import Language.LSP.Protocol.Types
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Util (aesonFromList)


tests :: TopSpec
tests = describe "Ruby" $ do
  kernelTests "ruby"
  kernelTests "ruby_3_0"
  kernelTests "ruby_3_1"
  kernelTests "ruby_3_2"


kernelTests :: Text -> TopSpec
kernelTests lang = do
  describe (T.unpack lang) $ introduceNixEnvironment [kernelSpec lang] [] [i|Ruby (#{lang})|] $ introduceJupyterRunner $ do
    testKernelSearchersNonempty lang

    testKernelStdout lang [__i|puts "hi"|] "hi\n"

    itHasHoverSatisfying "solargraph" "test.rb" Nothing [__i|puts "hi"|] (Position 0 2) $ \hover -> do
      let InL (MarkupContent MarkupKind_Markdown text) = hover ^. contents
      text `textShouldContain` "Kernel#puts"
      text `textShouldContain` "$stdout.puts(obj"
      text `textShouldContain` "Returns:"

kernelSpec :: Text -> NixKernelSpec
kernelSpec lang = NixKernelSpec {
  nixKernelName = lang
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just [i|Ruby (#{lang})|]
  , nixKernelPackages = []
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Just $ aesonFromList [
      ("lsp.solargraph.enable", A.Bool True)
      ]
  }

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
