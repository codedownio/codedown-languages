
module Spec.Tests.Haskell.DocumentHighlight (
  tests

  , documentHighlightCode
  , documentHighlightResults
  ) where

import Data.String.Interpolate
import Data.Text as T
import Language.LSP.Protocol.Types
import Language.LSP.Test hiding (message)
import qualified Language.LSP.Test.Helpers as Helpers
import Spec.Tests.Haskell.Common
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = describe "Document highlight" $ do
  it "foo (.ipynb)" $ doNotebookSession lsName documentHighlightCode $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName "haskell"
    getHighlights ident (Position 0 1) >>= (`shouldBe` documentHighlightResults)

  it "foo (.hs)" $ doSession' "Test.hs" lsName documentHighlightCodeRegular $ \(Helpers.LspSessionInfo {..}) -> do
    ident <- openDoc lspSessionInfoFileName "haskell"
    getHighlights ident (Position 0 1) >>= (
      `shouldBe`  [
          DocumentHighlight (Range (Position 0 0) (Position 0 3)) (Just DocumentHighlightKind_Write)
          , DocumentHighlight (Range (Position 2 11) (Position 2 14)) (Just DocumentHighlightKind_Read)
          ]
      )

documentHighlightCode :: Text
documentHighlightCode = [__i|foo = "hello"
                             putStrLn foo|]

documentHighlightResults :: [DocumentHighlight]
documentHighlightResults = [
  DocumentHighlight (Range (Position 0 0) (Position 0 3)) (Just DocumentHighlightKind_Write)
  , DocumentHighlight (Range (Position 1 9) (Position 1 12)) (Just DocumentHighlightKind_Read)
  ]

documentHighlightCodeRegular :: Text
documentHighlightCodeRegular = [__i|foo = "hello"
                                    main = do
                                      putStrLn foo|]

-------------------------------------

-- main :: IO ()
-- main = runSandwichWithCommandLineArgs Sandwich.defaultOptions $ do
--   introduceNixEnvironment [kernelSpec "ghc92"] [] "Haskell" $ do
--     tests
