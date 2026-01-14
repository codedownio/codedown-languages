module Spec.Tests.Cpp.Completion (tests) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Unlift
import Data.String.Interpolate
import Data.Text (Text)
import Language.LSP.Protocol.Lens hiding (edit, item, range, length)
import Language.LSP.Protocol.Types
import Language.LSP.Test
import qualified Language.LSP.Test.Helpers as Helpers
import Spec.Tests.Cpp.Common
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits (waitUntil)
import TestLib.LSP
import TestLib.Types
import UnliftIO.Exception


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Completions" $ do
  forM_ ["main.ipynb", "test.cpp"] $ \doc -> do
    it [i|provides std:: completions (#{doc})|] $ doSession' doc lsName stdCompletionCode $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName LanguageKind_CPP

      waitUntil 60 $
        handle handleSessionException' $ do
          completions <- getCompletions ident (Position 2 5)
          info [i|Got completions: #{length completions} items|]
          let completionLabels = map (^. label) completions
          completionLabels `shouldContain` ["cout"]
          completionLabels `shouldContain` ["vector"]

    it [i|provides local variable completions (#{doc})|] $ doSession' doc lsName localVarCode $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName LanguageKind_CPP

      waitUntil 60 $
        handle handleSessionException' $ do
          completions <- getCompletions ident (Position 2 2)
          info [i|Got completions: #{length completions} items|]
          let completionLabels = map (^. label) completions
          completionLabels `shouldContain` ["myVariable"]
          completionLabels `shouldContain` ["myDouble"]


stdCompletionCode :: Text
stdCompletionCode = [__i|\#include <iostream>
                         \#include <vector>
                         std::|]

localVarCode :: Text
localVarCode = [__i|int myVariable = 42;
                    double myDouble = 3.14;
                    my|]

-- | Handle potential session exceptions
handleSessionException' :: MonadIO m => SessionException -> m ()
handleSessionException' (UnexpectedResponseError lspId err) = expectationFailure [i|LSP UnexpectedResponseError: #{lspId}, #{err}|]
handleSessionException' x = throwIO x
