{-# LANGUAGE RankNTypes #-}

module Spec.Tests.Ruby (tests) where

import Control.Lens
import Data.String.Interpolate
import Data.Text as T
import Language.LSP.Types
import Language.LSP.Types.Lens
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes


tests :: TopSpec
tests = describe "Ruby" $ do
  rubyTests "ruby"
  rubyTests "ruby_2_7"
  rubyTests "ruby_3_1"


rubyTests :: Text -> TopSpec
rubyTests lang = do
  describe (T.unpack lang) $ introduceNixEnvironment [kernelSpec lang] [] [i|Ruby (#{lang})|] $ introduceJupyterRunner $ do
    testKernelStdout "ruby" [__i|puts "hi"|] "hi\n"

    itHasHoverSatisfying "solargraph" "test.rb" [__i|puts "hi"|] (Position 0 2) $ \hover ->
      (hover ^. contents)
        `shouldBe` HoverContents
          MarkupContent
            { _kind = MkMarkdown,
              _value = "Kernel#puts\n\n(*args) => nil\n\nEquivalent to\n\n    $stdout.puts(obj, ...)\n\n\n\nReturns:\n* [nil] \n\nVisibility: public"
            }

kernelSpec lang = NixKernelSpec {
  nixKernelChannel = "codedown"
  , nixKernelLanguage = lang
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
