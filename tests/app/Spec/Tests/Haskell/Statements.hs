
module Spec.Tests.Haskell.Statements where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.String.Interpolate
import Data.Text as T
import Language.LSP.Test hiding (message)
import Language.LSP.Types
import Spec.Tests.Haskell.Common
import Spec.Tests.Haskell.DocumentHighlight
import Test.Sandwich as Sandwich
import TestLib.LSP


statementsTests :: (
  Sandwich.HasLabel context "nixEnvironment" FilePath, HasBaseContext context, MonadBaseControl IO m, MonadUnliftIO m, MonadThrow m
  ) => SpecFree context m ()
statementsTests = describe "Statements" $ do
  describe "Single-line" $ do
    it "doesn't choke" $ doNotebookSession lsName statementsCode $ \filename -> do
      ident <- openDoc filename "haskell"
      getHighlights ident (Position 0 1) >>= (`shouldBe` List documentHighlightResults)

    testDiagnostics lsName "main.ipynb" statementsCode $ \diagnostics -> do
      -- Note: normally the server wouldn't send empty diagnostics. But the statement inserts "= unsafePerformIO $ ",
      -- which causes it to emit a "redundant bracket" diagnostic, which then gets filtered out by untransformPosition
      diagnostics `shouldBe` []

  describe "Multi-line" $ do
    it "doesn't choke" $ doNotebookSession lsName statementsCode $ \filename -> do
      ident <- openDoc filename "haskell"
      getHighlights ident (Position 0 1) >>= (`shouldBe` List documentHighlightResults)

    testDiagnostics lsName "main.ipynb" statementsCodeMultiline $ \diagnostics -> do
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
