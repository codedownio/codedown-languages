{-# LANGUAGE OverloadedStrings #-}

module Spec.Tests.Java.Lsp.Hovers (tests) where

import Data.String.Interpolate
import Data.Text (Text)
import Language.LSP.Protocol.Types
import Language.LSP.Test
import qualified Language.LSP.Test.Helpers as Helpers
import Spec.Tests.Java.Common
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits (waitUntil)
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Hovers" $ do
  -- Naming the synthetic class is the proof the projection is what got compiled.
  it "hovers a method declared in a cell" $ doNotebookSession lsName notebookCode $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName LanguageKind_Java

    waitUntil projectTimeout $ do
      hover <- getHoverOrException ident (Position 4 6)
      allHoverText hover `textShouldContain` [i|__Cells.doubled(int v)|]

  it "hovers a package method in a cell" $ doNotebookSession lsName notebookPackageCode $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName LanguageKind_Java

    waitUntil projectTimeout $ do
      hover <- getHoverOrException ident (Position 2 30)
      allHoverText hover `textShouldContain` [i|org.apache.commons.lang3.StringUtils.capitalize|]

  it "hovers a local method" $ doSession' "test.java" lsName localCode $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName LanguageKind_Java

    waitUntil projectTimeout $ do
      hover <- getHoverOrException ident (Position 5 12)
      allHoverText hover `textShouldContain` [i|int Example.doubled(int x)|]

  -- Resolved out of the jar, so referencedLibraries reached JDT LS.
  it "hovers a method from an environment package" $ doSession' "test.java" lsName packageCode $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName LanguageKind_Java

    waitUntil projectTimeout $ do
      hover <- getHoverOrException ident (Position 4 25)
      allHoverText hover `textShouldContain` [i|org.apache.commons.lang3.StringUtils.capitalize|]

-- One cell, in an order no .java file allows.
notebookCode :: Text
notebookCode = "import java.util.List;\n\nint x = 41;\n\nint doubled(int v) {\n    return v * 2;\n}\n\nSystem.out.println(doubled(x));\nx + 1"

notebookPackageCode :: Text
notebookPackageCode = "import org.apache.commons.lang3.StringUtils;\n\nString greet = StringUtils.capitalize(\"hi\");"

localCode :: Text
localCode = "class Example {\n  int doubled(int x) {\n    return x * 2;\n  }\n\n  int y = doubled(21);\n}\n"

packageCode :: Text
packageCode = "import org.apache.commons.lang3.StringUtils;\n\nclass Example {\n  String greet(String name) {\n    return StringUtils.capitalize(name);\n  }\n}\n"
