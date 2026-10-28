
module Spec.Tests.Rust.LSP.Completion (tests) where

import Control.Lens
import Control.Monad
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
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

  -- serialize only exists if the proc-macro server expanded the derive.
  it "(main.ipynb) Completes a serde-derived method" $ doSession' "main.ipynb" "rust-analyzer" derivedMethodCode $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName LanguageKind_Rust

    waitUntil 120 $ do
      labels <- fmap (^. label) <$> getCompletions ident (Position 4 5)
      unless (any ("serialize" `T.isPrefixOf`) labels) $
        expectationFailure [i|No serialize completion. Had: #{labels}|]

derivedMethodCode :: Text
derivedMethodCode = [__i|use serde::Serialize;
                         \#[derive(Serialize)]
                         struct Foo { x: u8 }
                         let f = Foo { x: 1 };
                         f.ser
                        |]

isPrintLnCompletion :: CompletionItem -> Bool
isPrintLnCompletion ci = case ci ^. textEdit of
  Nothing -> False
  Just (InL x) -> x ^. newText == "println!($0)"
  Just (InR x) -> x ^. newText == "println!($0)"
