module Spec.Tests.Haskell.Formatting (tests) where

import Language.LSP.Protocol.Types
import Spec.Tests.Haskell.Common
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.Types


-- | ormolu is the default formattingProvider; see
-- kernels.haskell.lsp.haskell-language-server.formattingProvider for the alternatives.
tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Formatting" $
  itFormatsAs' 300 lsName "Test.hs" LanguageKind_Haskell
    "module Main where\nmain   ::   IO ()\nmain =  putStrLn    \"hi\"\n"
    "module Main where\n\nmain :: IO ()\nmain = putStrLn \"hi\"\n"
