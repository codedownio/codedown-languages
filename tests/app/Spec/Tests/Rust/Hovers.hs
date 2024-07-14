
module Spec.Tests.Rust.Hovers where

import Control.Monad
import Control.Monad.IO.Unlift
import Data.String.Interpolate
import Language.LSP.Protocol.Types
import Language.LSP.Test
import Test.Sandwich as Sandwich
import Test.Sandwich.Contexts.Waits (waitUntil)
import TestLib.LSP
import UnliftIO.Exception


hoverTests :: (LspContext context m) => SpecFree context m ()
hoverTests = describe "Hovers" $ do
  forM_ ["main.ipynb", "test.rs"] $ \doc -> do
    it [i|hovers println! (#{doc})|] $ doSession' doc "rust-analyzer" [i|println!("hi")|] $ \filename -> do
      ident <- openDoc filename "haskell"

      waitUntil 60 $
        handle handleSessionException $ do
          hover <- getHoverOrException ident (Position 0 1)
          allHoverText hover `textShouldContain` [i|Prints to the standard output|]



-- | We may get an UnexpectedResponseError from rust-analyzer, while it's indexing (?)
-- Not sure why, but rethrow it as a sandwich FailureReason so the waitUntil keeps trying.
handleSessionException :: MonadIO m => SessionException -> m ()
handleSessionException (UnexpectedResponseError lspId err) = expectationFailure [i|LSP UnexpectedResponseError: #{lspId}, #{err}|]
handleSessionException x = throwIO x
