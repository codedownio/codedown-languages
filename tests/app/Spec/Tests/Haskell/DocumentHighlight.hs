
module Spec.Tests.Haskell.DocumentHighlight (
  documentHighlightTests

  , documentHighlightCode
  , documentHighlightResults
  ) where

import Data.String.Interpolate
import Data.Text as T
import Language.LSP.Protocol.Types
import Language.LSP.Test hiding (message)
import Spec.Tests.Haskell.Common
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.NixEnvironmentContext


documentHighlightTests :: (LspContext context m) => SpecFree context m ()
documentHighlightTests = describe "Document highlight" $ do
  it "highlights foo" $ doNotebookSession lsName documentHighlightCode $ \filename -> do
    ident <- openDoc filename "haskell"
    getHighlights ident (Position 0 1) >>= (`shouldBe` documentHighlightResults)

  it "highlights foo in a regular doc" $ doSession' "Test.hs" lsName documentHighlightCodeRegular $ \filename -> do
    ident <- openDoc filename "haskell"
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

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions $ do
  introduceNixEnvironment [kernelSpec "haskell-ghc92"] [] "Haskell" $ do
    documentHighlightTests
