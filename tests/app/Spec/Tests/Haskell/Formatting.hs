module Spec.Tests.Haskell.Formatting (tests) where

import Language.LSP.Protocol.Types
import Spec.Tests.Haskell.Common
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.Types


-- | ormolu is the default formattingProvider; see
-- kernels.haskell.lsp.haskell-language-server.formattingProvider for the alternatives.
tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Formatting" $ do
  itFormatsAs' 300 lsName "Test.hs" LanguageKind_Haskell
    "module Main where\nmain   ::   IO ()\nmain =  putStrLn    \"hi\"\n"
    "module Main where\n\nmain :: IO ()\nmain = putStrLn \"hi\"\n"

  it "haskell-language-server formats main.ipynb" $ do
    -- haskell-notebook-language-server forwards textDocument/formatting with the notebook's
    -- URI, and HLS gates its formatters on the file extension: "ormolu does not support
    -- .ipynb filetypes". See plans/notebook-formatting-in-proxies.md.
    _ <- pending
    formatsAs 300 lsName "main.ipynb" LanguageKind_Haskell
      "foo   ::   IO ()\nfoo =  putStrLn    \"hi\"\n"
      "foo :: IO ()\nfoo = putStrLn \"hi\"\n"
