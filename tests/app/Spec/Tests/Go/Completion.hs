module Spec.Tests.Go.Completion (tests) where

import Control.Lens
import Control.Monad.IO.Unlift
import qualified Data.List as L
import Data.String.Interpolate
import Data.Text (Text)
import Language.LSP.Protocol.Lens
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
  describe "main.ipynb" $ do
    it "provides fmt. completions" $ doSession' "main.ipynb" lsName fmtCompletionCodeNotebook $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName LanguageKind_Go

      waitUntil 60 $ do
        completions <- getCompletions ident (Position 1 4)
        info [i|Got completions: #{completions}|]
        let insertTexts = fmap (^. label) completions
        insertTexts `listShouldContain` "Println"
        insertTexts `listShouldContain` "Printf"

    it "provides local variable completions" $ doSession' "main.ipynb" lsName localVarCodeNotebook $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName LanguageKind_Go

      waitUntil 60 $ do
        completions <- getCompletions ident (Position 2 2)
        info [i|Got completions: #{completions}|]
        let insertTexts = fmap (^. label) completions
        insertTexts `listShouldContain` "myVariable"
        insertTexts `listShouldContain` "myFloat"

  describe "test.go" $ do
    it "provides fmt. completions" $ doSession' "test.go" lsName fmtCompletionCodeStandalone $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName LanguageKind_Go

      waitUntil 60 $ do
        completions <- getCompletions ident (Position 3 8)
        info [i|Got completions: #{completions}|]
        let insertTexts = fmap (^. label) completions
        insertTexts `listShouldContain` "Println"
        insertTexts `listShouldContain` "Printf"

    it "provides local variable completions" $ doSession' "test.go" lsName localVarCodeStandalone $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName LanguageKind_Go

      waitUntil 60 $ do
        completions <- getCompletions ident (Position 4 6)
        info [i|Got completions: #{completions}|]
        let insertTexts = fmap (^. label) completions
        insertTexts `listShouldContain` "myVariable"
        insertTexts `listShouldContain` "myFloat"

fmtCompletionCodeNotebook :: Text
fmtCompletionCodeNotebook = [__i|import "fmt"
                                 fmt.|]

localVarCodeNotebook :: Text
localVarCodeNotebook = [__i|myVariable := 42
                            myFloat := 3.14
                            my|]

fmtCompletionCodeStandalone :: Text
fmtCompletionCodeStandalone = [__i|package main
                                   import "fmt"
                                   func main() {
                                       fmt.
                                   }|]

localVarCodeStandalone :: Text
localVarCodeStandalone = [__i|package main
                              func main() {
                                  myVariable := 42
                                  myFloat := 3.14
                                  my
                              }|]

listShouldContain :: (MonadIO m, Eq a, Show a) => [a] -> a -> m ()
listShouldContain haystack needle = case L.elem needle haystack of
  True -> return ()
  False -> expectationFailure [i|Expected list to contain #{show needle}, but had: #{show haystack}|]
