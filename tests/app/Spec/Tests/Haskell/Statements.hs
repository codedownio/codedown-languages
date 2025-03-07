
module Spec.Tests.Haskell.Statements (tests) where

import Control.Monad
import Data.String.Interpolate
import Data.Text as T
import Language.LSP.Protocol.Types
import Language.LSP.Test hiding (message)
import Spec.Tests.Haskell.Common
import Spec.Tests.Haskell.DocumentHighlight (documentHighlightResults)
import Test.Sandwich as Sandwich
import TestLib.LSP
import UnliftIO.Timeout


tests :: (LspContext context m) => Text -> SpecFree context m ()
tests ghcPackage = describe "Statements" $ do
  describe "Single-line" $ do
    it "doesn't choke" $ doNotebookSession lsName statementsCode $ \filename -> do
      ident <- openDoc filename "haskell"
      timeout 120_000_000 (getHighlights ident (Position 0 1)) >>= (`shouldBe` (Just documentHighlightResults))

    when (ghcPackage /= "ghc98") $ -- TODO: re-enable hlint test with haskell-language-server 2.8.0.0
      testDiagnosticsLabel "Empty diagnostics" lsName "main.ipynb" Nothing statementsCode $ \diagnostics -> do
        -- Note: normally the server wouldn't send empty diagnostics. But the statement inserts "= unsafePerformIO $ ",
        -- which causes it to emit a "redundant bracket" diagnostic, which then gets filtered out by untransformPosition
        diagnostics `shouldBe` []

  describe "Multi-line" $ do
    it "doesn't choke" $ doNotebookSession lsName statementsCode $ \filename -> do
      ident <- openDoc filename "haskell"
      timeout 120_000_000 (getHighlights ident (Position 0 1)) >>= (`shouldBe` (Just documentHighlightResults))

    when (ghcPackage /= "ghc98") $ -- TODO: re-enable hlint test with haskell-language-server 2.8.0.0
      testDiagnosticsLabel "Redundant bracket" lsName "main.ipynb" Nothing statementsCodeMultiline $ \diagnostics -> do
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
