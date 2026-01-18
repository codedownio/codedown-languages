module Spec.Tests.Cpp.Completion (tests) where

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
import Spec.Tests.Cpp.Common
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits (waitUntil)
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Completions" $ do
  forM_ ["main.ipynb", "test.cpp"] $ \doc -> describe (T.unpack doc) $ do
    it [i|provides std:: completions|] $ doSession' doc lsName stdCompletionCode $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName LanguageKind_CPP

      waitUntil 60 $ do
        completions <- getCompletions ident (Position 2 5)
        info [i|Got completions: #{completions}|]
        let insertTexts = mapMaybe _insertText completions
        insertTexts `listShouldContain` "iostream"
        insertTexts `listShouldContain` "vector"

    it [i|provides local variable completions|] $ doSession' doc lsName localVarCode $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName LanguageKind_CPP

      waitUntil 60 $ do
        completions <- getCompletions ident (Position 2 2)
        info [i|Got completions: #{completions}|]
        let insertTexts = mapMaybe _insertText completions
        insertTexts `listShouldContain` "myVariable"
        insertTexts `listShouldContain` "myDouble"

stdCompletionCode :: Text
stdCompletionCode = [__i|\#include <iostream>
                         \#include <vector>
                         std::|]

localVarCode :: Text
localVarCode = [__i|int myVariable = 42;
                    double myDouble = 3.14;
                    my|]

listShouldContain :: (MonadIO m, Eq a, Show a) => [a] -> a -> m ()
listShouldContain haystack needle = case L.elem needle haystack of
  True -> return ()
  False -> expectationFailure [i|Expected list to contain #{show needle}, but had: #{show haystack}|]
