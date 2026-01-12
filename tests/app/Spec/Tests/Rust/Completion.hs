
module Spec.Tests.Rust.Completion (tests) where

import Control.Lens
import Control.Monad
import Data.String.Interpolate
import Language.LSP.Protocol.Lens hiding (edit, item, range)
import Language.LSP.Protocol.Types
import Language.LSP.Test
import qualified Language.LSP.Test.Helpers as Helpers
import Safe
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits (waitUntil)
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Completions" $ do
  forM_ ["main.ipynb", "test.rs"] $ \doc -> do
    it [i|(#{doc}) Completes printl to println!|] $ doSession' doc "rust-analyzer" [i|printl|] $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName "haskell"

      waitUntil 60 $ do
        completions <- getCompletions ident (Position 0 6)

        edit <- case headMay (filter isPrintLnCompletion completions) of
          Nothing -> expectationFailure [i|Couldn't find println! response|]
          Just item -> pure (item ^. textEdit)

        let range = Range (Position 0 0) (Position 0 6)
        edit `shouldBe` Just (InR (InsertReplaceEdit "println!($0)" range range))

isPrintLnCompletion :: CompletionItem -> Bool
isPrintLnCompletion ci = case ci ^. textEdit of
  Nothing -> False
  Just (InL x) -> x ^. newText == "println!($0)"
  Just (InR x) -> x ^. newText == "println!($0)"
