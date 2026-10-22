{-# LANGUAGE OverloadedStrings #-}

module Spec.Tests.Java.Lsp.Completion (tests) where

import Control.Lens
import Data.String.Interpolate
import Data.Text (Text)
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Types
import Language.LSP.Test
import qualified Language.LSP.Test.Helpers as Helpers
import Spec.Tests.Java.Common
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits (waitUntil)
import TestLib.LSP
import TestLib.Types


-- JDT LS puts the signature in the label, so a completion reads "doubled(int x) : int" rather
-- than just the name.
tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Completions" $ do
  it "completes a local method" $ doSession' "test.java" lsName localCode $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName LanguageKind_Java

    waitUntil projectTimeout $ do
      completions <- getCompletions ident (Position 5 12)
      info [i|Got completions: #{completions}|]
      fmap (^. label) completions `listShouldContain` "doubled(int x) : int"

  -- This one only works because JDT LS was pointed at the environment's jars: without them
  -- StringUtils doesn't resolve and these completions are just the words already in the file.
  it "completes a method from an environment package" $ doSession' "test.java" lsName packageCode $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName LanguageKind_Java

    waitUntil projectTimeout $ do
      completions <- getCompletions ident (Position 4 24)
      info [i|Got completions: #{completions}|]
      fmap (^. label) completions `listShouldContain` "capitalize(String str) : String"

localCode :: Text
localCode = "class Example {\n  int doubled(int x) {\n    return x * 2;\n  }\n\n  int y = doubled(21);\n}\n"

packageCode :: Text
packageCode = "import org.apache.commons.lang3.StringUtils;\n\nclass Example {\n  String greet(String name) {\n    return StringUtils.capitalize(name);\n  }\n}\n"
