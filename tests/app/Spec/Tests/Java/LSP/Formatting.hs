module Spec.Tests.Java.LSP.Formatting (tests) where

import Data.Text (Text)
import Language.LSP.Protocol.Types
import Spec.Tests.Java.Common
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = itFormatsAs lsName "test.java" LanguageKind_Java badlyFormattedCode formattedCode

badlyFormattedCode :: Text
badlyFormattedCode = "class Example {\nint   x =1+2;\nint doubled( ){\nreturn x*2;\n}\n}\n"

formattedCode :: Text
formattedCode = "class Example {\n  int x = 1 + 2;\n\n  int doubled() {\n    return x * 2;\n  }\n}\n"
