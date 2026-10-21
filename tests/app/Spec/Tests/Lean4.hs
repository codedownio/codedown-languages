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
          [out] -> do
            out `textShouldContain` "▶  4"
            -- patch-lean4-jupyter.py strips the repl's JSON from text/plain.
            out `textShouldNotContain` "Raw output:"
          xs -> expectationFailure [i|Expected one output, got: #{xs}|]

      it "proves a theorem with no complaint" $
        plainOutputsShouldSatisfy "theorem t : 1 + 1 = 2 := by decide" $ \case
          [out] -> out `textShouldNotContain` "❌"
          xs -> expectationFailure [i|Expected one output, got: #{xs}|]

      it "reports a failing proof" $
        plainOutputsShouldSatisfy "theorem t : 1 + 1 = 3 := by decide" $ \case
          [out] -> out `textShouldContain` "Tactic `decide` proved that the proposition"
          xs -> expectationFailure [i|Expected one output, got: #{xs}|]

      -- The goal line comes from patch-lean4-jupyter.py; Alectryon never renders it.
      it "surfaces the sorry warning and the open goal" $
        plainOutputsShouldSatisfy "theorem t : 1 + 1 = 2 := by sorry" $ \case
          [out] -> do
            out `textShouldContain` "declaration uses `sorry`"
            out `textShouldContain` "proofState 0:"
            out `textShouldContain` "⊢ 1 + 1 = 2"
          xs -> expectationFailure [i|Expected one output, got: #{xs}|]

      -- Env chaining is what breaks first when the repl commit is re-picked.
      it "carries definitions across cells" $
        plainOutputsShouldSatisfy [__i|def x := 41
                                       @@@CELL@@@
                                       \#eval x + 1
                                      |] $ \case
          [_, second] -> second `textShouldContain` "▶  42"
          xs -> expectationFailure [i|Expected two outputs, got: #{xs}|]

    -- lean4_jupyter's own additions over a bare repl wrapper, and the first thing that
    -- would rot silently given upstream is unmaintained. A leading `%` becomes `--%`, so
    -- the magic line stays a Lean comment in whatever gets sent.
    describe "Magics" $ do
      -- Sends {"tactic": ..., "proofState": 0} rather than a cmd, picking the goal the
      -- previous cell left open.
      it "discharges a sorried goal with % proof" $
        plainOutputsShouldSatisfy [__i|theorem t : 1 + 1 = 2 := by sorry
                                       @@@CELL@@@
                                       % proof 0
                                       decide
                                      |] $ \case
          [_, closed] -> closed `textShouldContain` "Goals accomplished"
          xs -> expectationFailure [i|Expected two outputs, got: #{xs}|]

      it "rewinds to an earlier environment with % env" $
        plainOutputsShouldSatisfy [__i|def inEnvZero := 41
                                       @@@CELL@@@
                                       def inEnvOne := 99
                                       @@@CELL@@@
                                       % env 0
                                       \#eval inEnvOne
                                       @@@CELL@@@
                                       % env 0
                                       \#eval inEnvZero
                                      |] $ \case
          [_, _, rewound, stillVisible] -> do
            rewound `textShouldContain` "Unknown identifier `inEnvOne`"
            stillVisible `textShouldContain` "▶  41"
          xs -> expectationFailure [i|Expected four outputs, got: #{xs}|]

    -- The server keys off content, not filename, so both paths should behave identically.
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

      -- No formatting test: Lean has no formatter.

  -- The kernel and the server get packages through different variables; check both.
  describe "Packages" $ introduceNixEnvironment [kernelSpecWithPackages] [] "Lean 4 with batteries" $ introduceJupyterRunner $ do
    it "imports a package in a cell" $
      plainOutputsShouldSatisfy unionFindCode $ \case
        [out] -> out `textShouldContain` "Batteries.UnionFind : Type"
        xs -> expectationFailure [i|Expected one output, got: #{xs}|]

    it "hovers a name from a package" $ doSession' "test.lean" lsName unionFindCode $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName leanKind

      waitUntil 180 $ do
        hover <- getHoverOrException ident (Position 1 20)
        allHoverText hover `textShouldContain` "Batteries.UnionFind : Type"

    -- Comes back empty without LEAN_SRC_PATH.
    it "jumps into a package's source" $ doSession' "test.lean" lsName unionFindCode $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName leanKind

      waitUntil 180 $ do
        defs <- getDefinitions ident (Position 1 20)
        case definitionUris defs of
          [Uri u] -> u `textShouldContain` "batteries"
          xs -> expectationFailure [i|Expected one definition target, got: #{xs}|]

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

kernelSpecWithPackages :: NixKernelSpec
kernelSpecWithPackages = kernelSpecWithLsp {
  nixKernelPackages = [nameOnly "batteries"]
  }

-- | `Batteries.UnionFind` isn't in core, so resolving it means the package is on the path.
unionFindCode :: Text
unionFindCode = [__i|import Batteries
                     \#check @Batteries.UnionFind
                    |]

-- | The kernel emits display_data, not stdout or an execute_result.
plainOutputsShouldSatisfy :: (
  HasJupyterRunnerContext context, JupyterRunnerMonad m
  ) => Text -> ([Text] -> ExampleT context m ()) -> ExampleT context m ()
plainOutputsShouldSatisfy code cb =
  displayDatasShouldSatisfy "lean4" code (cb . mimeTexts (MimeType "text/plain"))

-- | First line elaborates cleanly, so a diagnostic on line 1 tests position mapping.
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
