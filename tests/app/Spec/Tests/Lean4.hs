{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Lean4 (tests) where

import Control.Lens hiding ((.=))
import Control.Monad
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import Language.LSP.Protocol.Lens (label)
import Language.LSP.Protocol.Types
import Language.LSP.Test
import qualified Language.LSP.Test.Helpers as Helpers
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits (waitUntil)
import TestLib.JupyterRunnerContext
import TestLib.JupyterTypes
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types
import TestLib.Util


tests :: LanguageSpec
tests = describe "Lean 4" $ do
  testKernelSearchersBuild "lean4"
  testHasExpectedFields "lean4"

  introduceNixEnvironment [kernelSpecWithLsp] [] "Lean 4" $ introduceJupyterRunner $ do
    describe "Kernel tests" $ do
      it "evaluates an expression" $
        plainOutputsShouldSatisfy "#eval 2 + 2" $ \case
          [out] -> out `textShouldContain` "▶  4"
          xs -> expectationFailure [i|Expected one output, got: #{xs}|]

      it "proves a theorem with no complaint" $
        plainOutputsShouldSatisfy "theorem t : 1 + 1 = 2 := by decide" $ \case
          -- The kernel renders every message it gets from the repl, so a proof that goes
          -- through is exactly the case where there's nothing to render.
          [out] -> out `textShouldNotContain` "❌"
          xs -> expectationFailure [i|Expected one output, got: #{xs}|]

      it "reports a failing proof" $
        plainOutputsShouldSatisfy "theorem t : 1 + 1 = 3 := by decide" $ \case
          [out] -> out `textShouldContain` "Tactic `decide` proved that the proposition"
          xs -> expectationFailure [i|Expected one output, got: #{xs}|]

      it "surfaces the sorry warning and the open goal" $
        plainOutputsShouldSatisfy "theorem t : 1 + 1 = 2 := by sorry" $ \case
          [out] -> do
            out `textShouldContain` "declaration uses `sorry`"
            out `textShouldContain` "⊢ 1 + 1 = 2"
          xs -> expectationFailure [i|Expected one output, got: #{xs}|]

      -- The repl chains each cell's environment onto the previous one's, which is the part
      -- most likely to break when the repl commit is re-picked on a nixpkgs bump.
      it "carries definitions across cells" $
        plainOutputsShouldSatisfy [__i|def x := 41
                                       @@@CELL@@@
                                       \#eval x + 1
                                      |] $ \case
          [_, second] -> second `textShouldContain` "▶  42"
          xs -> expectationFailure [i|Expected two outputs, got: #{xs}|]

    -- `lean --server` takes the document as-is rather than keying off the filename, so the
    -- notebook needs no proxy: a concatenation of cells is already a valid .lean file, and
    -- positions come back on the notebook's own lines. Run everything against both to pin
    -- that down.
    describe "LSP" $ forM_ ["test.lean", "main.ipynb"] $ \filename -> describe filename $ do
      testDiagnosticsLabel [i|#{lsName}: failing proof on the second line|] lsName filename leanKind
        failingProofOnSecondLine $ \diagnostics ->
          assertDiagnosticRanges diagnostics [(Range (Position 1 28) (Position 1 34), Nothing)]

      it "hovers a constant from the prelude" $ doSession' (T.pack filename) lsName "#eval Nat.succ 1" $ \(Helpers.LspSessionInfo {..}) -> do
        ident <- openDoc lspSessionInfoFileName leanKind

        waitUntil 120 $ do
          hover <- getHoverOrException ident (Position 0 8)
          allHoverText hover `textShouldContain` "Nat.succ (n : Nat) : Nat"

      it "completes after Nat." $ doSession' (T.pack filename) lsName "#eval Nat." $ \(Helpers.LspSessionInfo {..}) -> do
        ident <- openDoc lspSessionInfoFileName leanKind

        waitUntil 120 $ do
          completions <- getCompletions ident (Position 0 10)
          fmap (^. label) completions `listShouldContain` "succ"

      it "jumps to a definition from an earlier cell" $ doSession' (T.pack filename) lsName definedThenUsed $ \(Helpers.LspSessionInfo {..}) -> do
        ident <- openDoc lspSessionInfoFileName leanKind

        waitUntil 120 $ do
          defs <- getDefinitions ident (Position 1 8)
          definitionRanges ident defs `shouldBe` [Range (Position 0 4) (Position 0 14)]

      -- No formatting test: Lean ships no formatter and the server advertises no
      -- documentFormattingProvider.

lsName :: Text
lsName = "lean"

leanKind :: LanguageKind
leanKind = LanguageKind_Custom "lean4"

kernelSpecWithLsp :: NixKernelSpec
kernelSpecWithLsp = NixKernelSpec {
  nixKernelName = "lean4"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "Lean 4"
  , nixKernelPackages = []
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelExtraConfig = Just [
      "lsp.lean.enable = true"
      ]
  }

-- | The kernel renders through Alectryon into display_data, not into stdout or an
-- execute_result, so every kernel assertion goes through the text/plain rendering.
plainOutputsShouldSatisfy :: (
  HasJupyterRunnerContext context, JupyterRunnerMonad m
  ) => Text -> ([Text] -> ExampleT context m ()) -> ExampleT context m ()
plainOutputsShouldSatisfy code cb =
  displayDatasShouldSatisfy "lean4" code (cb . mimeTexts (MimeType "text/plain"))

-- | The first line elaborates cleanly, so a diagnostic on line 1 is the real test that
-- positions survive the trip through the notebook.
failingProofOnSecondLine :: Text
failingProofOnSecondLine = [__i|def answer : Nat := 41
                                theorem t : 1 + 1 = 3 := by decide
                               |]

definedThenUsed :: Text
definedThenUsed = [__i|def myLocalDef : Nat := 41
                       \#eval myLocalDef
                      |]

main :: IO ()
main = jupyterMain tests
