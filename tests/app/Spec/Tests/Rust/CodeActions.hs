module Spec.Tests.Rust.CodeActions (tests) where

import Control.Lens hiding (List)
import Control.Monad.IO.Unlift
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack
import Language.LSP.Protocol.Lens hiding (actions, to)
import Language.LSP.Protocol.Types
import Language.LSP.Test hiding (message)
import qualified Language.LSP.Test.Helpers as Helpers
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits (waitUntil)
import TestLib.LSP
import TestLib.Types
import UnliftIO.Exception


-- | rust-notebook-language-server used to return a code action's edit in the shadow file's
-- coordinates -- its uri, its version, and lines inside the generated `fn main`. codedown
-- applies edits by document name, so it refused them outright.
tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Code actions" $ do
  it "edits the notebook, not the shadow file" $
    doSession' "main.ipynb" lsName oneCell $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName "rust"

      waitUntil 300 $ handle handleSessionException' $ do
        actions <- getCodeActions ident (Range (Position 0 4) (Position 0 8))
        edits <- editsOf ident insertExplicitType actions
        edits `shouldBe` [(Range (Position 0 8) (Position 0 8), ": Vec<i32>")]

  -- A second cell, so a correct untransform has to map lines rather than subtract a constant.
  it "maps a line in the second cell" $
    doSession' "main.ipynb" lsName twoCells $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName "rust"

      waitUntil 300 $ handle handleSessionException' $ do
        actions <- getCodeActions ident (Range (Position 2 4) (Position 2 8))
        edits <- editsOf ident insertExplicitType actions
        edits `shouldBe` [(Range (Position 2 8) (Position 2 8), ": Vec<i32>")]

-- | The edits the named action makes to this document, failing if it touches any other.
editsOf :: (
  HasCallStack, MonadIO m
  ) => TextDocumentIdentifier -> Text -> [Command |? CodeAction] -> m [(Range, Text)]
editsOf ident wanted actions = case L.find ((== wanted) . getTitle) actions of
  Nothing -> expectationFailure [i|No '#{wanted}' action. Got: #{fmap getTitle actions}|]
  Just (InL _command) -> expectationFailure [i|'#{wanted}' came back as a bare Command|]
  Just (InR action) -> case action ^. edit of
    Nothing -> expectationFailure [i|'#{wanted}' has no edit|]
    Just wsEdit -> do
      let documents = documentsOf wsEdit
      documents `shouldBe` [ident ^. uri]
      return (editsIn wsEdit)

documentsOf :: WorkspaceEdit -> [Uri]
documentsOf wsEdit =
  M.keys (fromMaybe mempty (wsEdit ^. changes))
  <> [tde ^. textDocument . uri | InL tde <- fromMaybe [] (wsEdit ^. documentChanges)]

editsIn :: WorkspaceEdit -> [(Range, Text)]
editsIn wsEdit =
  [(e ^. range, e ^. newText) | es <- M.elems (fromMaybe mempty (wsEdit ^. changes)), e <- es]
  <> [asPair e | InL tde <- fromMaybe [] (wsEdit ^. documentChanges), e <- tde ^. edits]
  where
    asPair (InL e) = (e ^. range, e ^. newText)
    asPair (InR e) = (e ^. range, e ^. newText)

getTitle :: (Command |? CodeAction) -> Text
getTitle (InL x) = x ^. title
getTitle (InR x) = x ^. title

lsName :: Text
lsName = "rust-analyzer"

insertExplicitType :: Text
insertExplicitType = "Insert explicit type `Vec<i32>`"

oneCell :: Text
oneCell = "let nums = vec![1, 2, 3];\nprintln!(\"{:?}\", nums);"

-- | codedown sends a notebook as its cells run together; the blank line stands in for the
-- boundary between two code cells.
twoCells :: Text
twoCells = T.intercalate "\n" [
  "let greeting = \"hi\";"
  , ""
  , "let nums = vec![1, 2, 3];"
  , "println!(\"{:?} {}\", nums, greeting);"
  ]

-- | rust-analyzer answers ContentModified while it's still indexing; let waitUntil retry.
handleSessionException' :: MonadIO m => SessionException -> m ()
handleSessionException' (UnexpectedResponseError lspId err) = expectationFailure [i|LSP UnexpectedResponseError: #{lspId}, #{err}|]
handleSessionException' x = throwIO x
