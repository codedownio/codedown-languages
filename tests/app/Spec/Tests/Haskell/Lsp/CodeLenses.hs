module Spec.Tests.Haskell.Lsp.CodeLenses (tests) where

import Control.Applicative
import Control.Lens ((^.), to)
import Control.Monad.Logger
import Control.Monad.IO.Unlift
import Data.String.Interpolate
import Data.Text as T
import Language.LSP.Protocol.Lens hiding (actions, executeCommand, to)
import Language.LSP.Protocol.Types
import Language.LSP.Test hiding (message)
import qualified Language.LSP.Test.Helpers as Helpers
import Spec.Tests.Haskell.Common
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits
import TestLib.LSP
import TestLib.Types


-- | HLS defers a lens's title to codeLens/resolve, and the resolve request carries no text
-- document. Until hnls started carrying the notebook uri in the lens data, the resolve reached
-- HLS with the lens's notebook range and either found nothing or the wrong binding.
tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Code lenses" $ do
  it "resolves a type signature lens onto its own line" $
    doNotebookSession lsName lensCode $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName "haskell"

      waitUntil 240 $ do
        titled <- resolvedTitles ident
        titled `shouldContain` [(1, "x :: Integer")]
        titled `shouldContain` [(3, "foo :: IO ()")]

  it "drops the lens on the binding it generated for a bare expression" $
    doNotebookSession lsName lensCode $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName "haskell"

      waitUntil 240 $ do
        titled <- resolvedTitles ident
        -- Line 0 is `putStrLn "HI"`, which hnls rewrites to a generated binding. Its
        -- add-signature lens has no notebook counterpart, so it should not come back at all.
        [t | (l, t) <- titled, l == 0] `shouldBe` []
        [t | (_, t) <- titled, "expr" `T.isPrefixOf` t] `shouldBe` []

  -- getDocumentEdit throws if the applyEdit names anything but the notebook.
  it "applies the lens's edit to the notebook, not the shadow document" $
    doNotebookSession lsName lensCode $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName "haskell"

      cmd <- waitUntil 240 $ do
        lenses <- getAndResolveCodeLenses ident
        case [c | lens <- lenses, lens ^. range . start . line == 1, Just c <- [lens ^. command]] of
          (c:_) -> return c
          [] -> expectationFailure [i|No resolved lens on line 1: #{lenses}|]

      executeCommand cmd
      edited <- getDocumentEdit ident

      T.lines edited `shouldBe` ["putStrLn \"HI\"", "x :: Integer", "x = 42", "", "foo = putStrLn \"HELLO\""]

-- | Each resolved lens as (line, title), dropping any that resolved without one.
resolvedTitles :: (MonadLoggerIO m, MonadUnliftIO m, Alternative m) => TextDocumentIdentifier -> Session m [(UInt, Text)]
resolvedTitles ident = do
  lenses <- getAndResolveCodeLenses ident
  return [(lens ^. range . start . line, title') | lens <- lenses, Just title' <- [lens ^. command . to (fmap (^. title))]]

lensCode :: Text
lensCode = [__i|putStrLn "HI"
                x = 42

                foo = putStrLn "HELLO"|]
