
module Spec.Tests.Rust.PreIndex (tests) where

import Control.Lens
import Control.Monad
import Data.String.Interpolate
import Language.LSP.Protocol.Lens hiding (edit, item, range)
import Language.LSP.Protocol.Types
import Language.LSP.Test
import qualified Language.LSP.Test.Helpers as Helpers
import System.FilePath
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits (waitUntil)
import TestLib.LSP
import TestLib.Types
import UnliftIO.Directory


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Pre-indexing" $ do
  it "Resolves the std sysroot offline, without fetching into the cargo registry" $
    doSession' "main.ipynb" "rust-analyzer" [i|printl|] $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName "rust"

      waitUntil 60 $ do
        completions <- getCompletions ident (Position 0 6)
        unless (any isPrintLnCompletion completions) $
          expectationFailure [i|Couldn't find println! completion (std sysroot not resolved?)|]

      let registryDir = lspSessionInfoHomeDir </> ".cargo" </> "registry"
      doesDirectoryExist registryDir >>= \case
        False -> return ()
        True -> do
          entries <- listDirectory registryDir
          expectationFailure [i|rust-analyzer populated the cargo registry at #{registryDir} (#{entries})|]

isPrintLnCompletion :: CompletionItem -> Bool
isPrintLnCompletion ci = case ci ^. textEdit of
  Nothing -> False
  Just (InL x) -> x ^. newText == "println!($0)"
  Just (InR x) -> x ^. newText == "println!($0)"
