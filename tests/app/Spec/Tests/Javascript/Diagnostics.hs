{-# LANGUAGE OverloadedStrings #-}

module Spec.Tests.Javascript.Diagnostics (tests) where

import qualified Data.List as L
import qualified Data.Text as T
import Data.String.Interpolate
import Language.LSP.Protocol.Lens (message)
import Language.LSP.Protocol.Types
import Control.Lens ((^.))
import Spec.Tests.Javascript.Common
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.Types


-- | tslab type checks JavaScript cells as well as TypeScript ones, so the language server is
-- configured with checkJs on. Without it the editor says nothing about an error that will stop
-- the cell from running.
tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Diagnostics" $ do
  testDiagnosticsLabel "reports an unknown name" lsName "test.js" LanguageKind_JavaScript unknownNameCode $ \diagnostics ->
    diagnostics `shouldHaveMessageContaining` "Cannot find name 'undefinedFn'"

  testDiagnosticsLabel "reports a type error inferred from a literal" lsName "test.js" LanguageKind_JavaScript typeErrorCode $ \diagnostics ->
    diagnostics `shouldHaveMessageContaining` "does not exist on type"

unknownNameCode :: T.Text
unknownNameCode = [__i|const x = 1;
                       undefinedFn();|]

typeErrorCode :: T.Text
typeErrorCode = [__i|const n = 5;
                     n.toUpperCase();|]

shouldHaveMessageContaining :: (Monad m, MonadFail m) => [Diagnostic] -> T.Text -> m ()
shouldHaveMessageContaining diagnostics needle =
  case L.find (\d -> needle `T.isInfixOf` (d ^. message)) diagnostics of
    Just _ -> return ()
    Nothing -> fail [i|Expected a diagnostic containing #{needle}, but got: #{fmap (^. message) diagnostics}|]
