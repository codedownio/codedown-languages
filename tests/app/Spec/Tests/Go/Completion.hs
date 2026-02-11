module Spec.Tests.Go.Completion (tests) where

import Control.Monad
import Control.Monad.IO.Unlift
import qualified Data.List as L
import Data.Maybe
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import Language.LSP.Protocol.Types
import Language.LSP.Test
import qualified Language.LSP.Test.Helpers as Helpers
import Spec.Tests.Go.Common
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits (waitUntil)
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Completions" $ do
  forM_ ["main.ipynb", "test.go"] $ \doc -> describe (T.unpack doc) $ do
    it [i|provides fmt. completions|] $ doSession' doc lsName fmtCompletionCode $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName LanguageKind_Go

      waitUntil 60 $ do
        completions <- getCompletions ident (Position 3 8)
        info [i|Got completions: #{completions}|]
        let insertTexts = mapMaybe _insertText completions
        insertTexts `listShouldContain` "Println"
        insertTexts `listShouldContain` "Printf"

    it [i|provides local variable completions|] $ doSession' doc lsName localVarCode $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName LanguageKind_Go

      waitUntil 60 $ do
        completions <- getCompletions ident (Position 3 6)
        info [i|Got completions: #{completions}|]
        let insertTexts = mapMaybe _insertText completions
        insertTexts `listShouldContain` "myVariable"
        insertTexts `listShouldContain` "myFloat"

fmtCompletionCode :: Text
fmtCompletionCode = [__i|import "fmt"
                         func test() {
                             fmt.
                         }|]

localVarCode :: Text
localVarCode = [__i|func test() {
                        myVariable := 42
                        myFloat := 3.14
                        my
                    }|]

listShouldContain :: (MonadIO m, Eq a, Show a) => [a] -> a -> m ()
listShouldContain haystack needle = case L.elem needle haystack of
  True -> return ()
  False -> expectationFailure [i|Expected list to contain #{show needle}, but had: #{show haystack}|]
