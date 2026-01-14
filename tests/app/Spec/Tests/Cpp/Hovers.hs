module Spec.Tests.Cpp.Hovers (tests) where

import Control.Monad
import Control.Monad.IO.Unlift
import Data.String.Interpolate
import Data.Text (Text)
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
tests = describe "Hovers" $ do
  forM_ ["main.ipynb", "test.cpp"] $ \doc -> do
    it [i|hovers std::cout (#{doc})|] $ doSession' doc lsName coutCode $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName LanguageKind_CPP

      waitUntil 60 $
        handle handleSessionException' $ do
          hover <- getHoverOrException ident (Position 1 5)
          allHoverText hover `textShouldContain` [i|std::ostream|]

    it [i|hovers variable declaration (#{doc})|] $ doSession' doc lsName varDeclCode $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName LanguageKind_CPP

      waitUntil 60 $
        handle handleSessionException' $ do
          hover <- getHoverOrException ident (Position 0 4)
          allHoverText hover `textShouldContain` [i|int|]

    it [i|hovers function call (#{doc})|] $ doSession' doc lsName sqrtCode $ \(Helpers.LspSessionInfo {..}) -> do
      ident <- openDoc lspSessionInfoFileName LanguageKind_CPP

      waitUntil 60 $
        handle handleSessionException' $ do
          hover <- getHoverOrException ident (Position 1 16)
          allHoverText hover `textShouldContain` [i|sqrt|]


coutCode :: Text
coutCode = [__i|\#include <iostream>
                std::cout << "hello" << std::endl;|]

varDeclCode :: Text
varDeclCode = [__i|int x = 42;
                   float y = 3.14;|]

sqrtCode :: Text
sqrtCode = [__i|\#include <cmath>
                double result = sqrt(16.0);|]

-- | Handle potential session exceptions
handleSessionException' :: MonadIO m => SessionException -> m ()
handleSessionException' (UnexpectedResponseError lspId err) = expectationFailure [i|LSP UnexpectedResponseError: #{lspId}, #{err}|]
handleSessionException' x = throwIO x
