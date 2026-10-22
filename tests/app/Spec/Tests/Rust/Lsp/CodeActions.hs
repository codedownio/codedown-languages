module Spec.Tests.Rust.Lsp.CodeActions (tests) where

import Control.Lens
import Control.Monad
import qualified Data.List as L
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text as T
import Language.LSP.Protocol.Lens hiding (actions, to)
import Language.LSP.Protocol.Types
import Language.LSP.Test hiding (message)
import qualified Language.LSP.Test.Helpers as Helpers
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits
import TestLib.LSP
import TestLib.Types
import UnliftIO.Exception (tryAny)


-- | rust-analyzer fills an action's edit in when it can and defers it to codeAction/resolve
-- when it can't, so both paths have to be mapped back to the notebook.
tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Code actions" $ do
  it "keeps an action's edits on the notebook, inline or resolved" $
    doNotebookSession lsName codeActionCode $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName "rust"

      -- rust-analyzer answers ContentModified until it has taken the document in, and lsp-test
      -- raises that rather than returning it.
      actions <- waitUntil 240 $ do
        acts <- tryAny (getCodeActions ident (Range (Position 0 4) (Position 0 5))) >>= \case
          Left err -> expectationFailure [i|textDocument/codeAction failed: #{err}|]
          Right acts -> return acts
        case [ca | InR ca <- acts] of
          [] -> expectationFailure [i|No code actions came back, only: #{fmap describeAction acts}|]
          cas -> return cas

      resolved <- forM actions $ \ca -> case ca ^. data_ of
        Just _ -> resolveCodeAction ca
        Nothing -> return ca

      -- An edit core can't map back is dropped, so Nothing is fine; a shadow uri is not.
      forM_ resolved $ \ca -> case ca ^. edit of
        Nothing -> return ()
        Just e -> L.nub (editUris e) `shouldBe` [ident ^. uri]

editUris :: WorkspaceEdit -> [Uri]
editUris (WorkspaceEdit changes documentChanges _) =
  maybe [] (L.concatMap documentChangeUris) documentChanges <> maybe [] M.keys changes

documentChangeUris :: (TextDocumentEdit |? (CreateFile |? (RenameFile |? DeleteFile))) -> [Uri]
documentChangeUris (InL textDocumentEdit) = [textDocumentEdit ^. textDocument . uri]
documentChangeUris (InR _fileOperation) = []

describeAction :: (Command |? CodeAction) -> Text
describeAction (InL c) = [i|command #{c ^. title}|]
describeAction (InR ca) = [i|action #{ca ^. title}|]

lsName :: Text
lsName = "rust-analyzer"

codeActionCode :: Text
codeActionCode = [__i|let x = 1 + 2;
                      println!("{}", x);|]
