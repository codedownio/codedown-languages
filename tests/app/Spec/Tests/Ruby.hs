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


kernelName :: Text -> Text
kernelName _rubyPackage = "ruby"

kernelTests :: Text -> LanguageSpec
kernelTests rubyPackage = do
  describe (T.unpack rubyPackage) $ introduceNixEnvironment [kernelSpec rubyPackage] [] [i|Ruby (#{rubyPackage})|] $ introduceJupyterRunner $ do
    testKernelSearchersNonempty (kernelName rubyPackage)
    testHasExpectedFields (kernelName rubyPackage)

    testKernelStdout (kernelName rubyPackage) [__i|puts "hi"|] "hi\n"

    when ("_" `T.isInfixOf` rubyPackage) $ do
      let versionString = rubyPackage
                        & T.drop 5
                        & T.replace "_" "."
      it [i|The Ruby version contains the string '#{versionString}'|] $ do
        testKernelStdout'' (kernelName rubyPackage) [__i|puts RUBY_VERSION|] $ \case
          Nothing -> expectationFailure [i|Got no stdout|]
          Just t -> do
            info [i|RUBY_VERSION result: #{t}|]
            t `textShouldContain` versionString

    itHasHoverSatisfying "solargraph" "test.rb" [__i|puts "hi"|] (Position 0 2) $ \hover -> do
      let InL (MarkupContent MarkupKind_Markdown text) = hover ^. contents
      text `textShouldContain` "Kernel#puts"
      text `textShouldContain` "$stdout.puts(obj"

kernelSpec :: Text -> NixKernelSpec
kernelSpec rubyPackage = NixKernelSpec {
  nixKernelName = kernelName rubyPackage
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just [i|Ruby (#{rubyPackage})|]
  , nixKernelPackages = []
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelExtraConfig = Just [
      "lsp.solargraph.enable = true"
      , [i|rubyPackage = "#{rubyPackage}"|]
      ]
  }

main :: IO ()
main = jupyterMain tests
