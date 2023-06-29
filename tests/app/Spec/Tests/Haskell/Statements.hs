
module Spec.Tests.Haskell.Statements where

import Data.String.Interpolate
import Data.Text as T
import Language.LSP.Protocol.Types
import Language.LSP.Test hiding (message)
import Spec.Tests.Haskell.Common
import Spec.Tests.Haskell.DocumentHighlight
import Test.Sandwich as Sandwich
import TestLib.LSP


statementsTests :: (LspContext context m) => SpecFree context m ()
statementsTests = describe "Statements" $ do
  describe "Single-line" $ do
    it "doesn't choke" $ doNotebookSession lsName statementsCode $ \filename -> do
      ident <- openDoc filename "haskell"
      getHighlights ident (Position 0 1) >>= (`shouldBe` documentHighlightResults)

    testDiagnostics lsName "main.ipynb" Nothing statementsCode $ \diagnostics -> do
      -- Note: normally the server wouldn't send empty diagnostics. But the statement inserts "= unsafePerformIO $ ",
      -- which causes it to emit a "redundant bracket" diagnostic, which then gets filtered out by untransformPosition
      diagnostics `shouldBe` []

  describe "Multi-line" $ do
    it "doesn't choke" $ doNotebookSession lsName statementsCode $ \filename -> do
      ident <- openDoc filename "haskell"
      getHighlights ident (Position 0 1) >>= (`shouldBe` documentHighlightResults)

    testDiagnostics lsName "main.ipynb" Nothing statementsCodeMultiline $ \diagnostics -> do
      info [i|Got diagnostics: #{diagnostics}|]
      assertDiagnosticRanges diagnostics [(Range (Position 1 9) (Position 1 14), Just (InR "refact:Redundant bracket"))]

statementsCode :: Text
statementsCode = [__i|foo = "hello"
                      putStrLn foo
                      import System.IO
                      :set -XScopedTypeVariables
                      num :: Int <- readLn
                      import Data.Aeson|]

statementsCodeMultiline :: Text
statementsCodeMultiline = [__i|foo = "hello"
                               putStrLn (foo)
                               import System.IO
                               :set -XScopedTypeVariables
                               num :: Int <- do
                                 readLn
                               import Data.Aeson
                              |]
