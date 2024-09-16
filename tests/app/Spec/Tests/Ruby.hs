{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Spec.Tests.Ruby (tests) where

import Control.Lens
import Control.Monad
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
import TestLib.Types


tests :: LanguageSpec
tests = describe "Ruby" $ do
  kernelTests "ruby"
  kernelTests "ruby_3_1"
  kernelTests "ruby_3_2"
  kernelTests "ruby_3_3"


kernelTests :: Text -> LanguageSpec
kernelTests lang = do
  describe (T.unpack lang) $ introduceNixEnvironment [kernelSpec lang] [] [i|Ruby (#{lang})|] $ introduceJupyterRunner $ do
    testKernelSearchersNonempty lang
    testHasExpectedFields lang

    testKernelStdout lang [__i|puts "hi"|] "hi\n"

    when ("_" `T.isInfixOf` lang) $ do
      let versionString = lang
                        & T.drop 5
                        & T.replace "_" "."
      it [i|The Ruby version contains the string '#{versionString}'|] $ do
        testKernelStdout'' lang [__i|puts RUBY_VERSION|] $ \case
          Nothing -> expectationFailure [i|Got no stdout|]
          Just t -> do
            info [i|RUBY_VERSION result: #{t}|]
            t `textShouldContain` versionString

    itHasHoverSatisfying "solargraph" "test.rb" Nothing [__i|puts "hi"|] (Position 0 2) $ \hover -> do
      let InL (MarkupContent MarkupKind_Markdown text) = hover ^. contents
      text `textShouldContain` "Kernel#puts"
      text `textShouldContain` "$stdout.puts(obj"

kernelSpec :: Text -> NixKernelSpec
kernelSpec lang = NixKernelSpec {
  nixKernelName = lang
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just [i|Ruby (#{lang})|]
  , nixKernelPackages = []
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Just [
      "lsp.solargraph.enable = true"
      ]
  }

main :: IO ()
main = jupyterMain tests
