{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}

module Spec.Tests.Haskell (tests) where

import Control.Lens ((^.))
import Control.Monad
import Control.Monad.Catch (MonadThrow, onException)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson as A
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Vector as V
import Language.LSP.Test hiding (message)
import Language.LSP.Types
import Language.LSP.Types.Lens
import Spec.Tests.Haskell.Diagnostics
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.JupyterTypes
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types (HasNixEnvironment)
import UnliftIO.Concurrent

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap          as HM
#else
import qualified Data.HashMap.Strict        as HM
#endif


tests :: TopSpec
tests = do
  -- See languages/haskell/default.nix for details on what's available

  -- haskellCommonTests "haskell-ghc865"
  -- haskellCommonTests "haskell-ghc884"

  haskellCommonTests "haskell-ghc8107"
  haskellCommonTests "haskell-ghc902"
  haskellCommonTests "haskell-ghc924"

  -- haskellCommonTests "haskell-ghc942"

haskellCommonTests :: Text -> TopSpec
haskellCommonTests lang = do
  describe [i|Haskell #{lang} with hlint output|] $ introduceNixEnvironment [kernelSpecWithHlintOutput lang] [] "Haskell" $ introduceJupyterRunner $ do
    describe "Kernel" $ do
      -- With the setting turned on, we should get hlint output
      itHasDisplayTexts lang etaExpandCode [Just (A.Array $ V.fromList [
                                                     String "Line 7: Eta reduce\n"
                                                     , String "Found:\n"
                                                     , String "baz2 x = baz x\n"
                                                     , String "Why not:\n"
                                                     , String "baz2 = baz"
                                                     ])]

  describe [i|Haskell #{lang}|] $ introduceNixEnvironment [kernelSpec lang] [] "Haskell" $ introduceJupyterRunner $ do
    testKernelSearchers lang

    describe "Kernel" $ do
      itHasDisplayDatas lang [__i|putStrLn "hi"|] [M.fromList [(MimeType "text/plain", A.Array (V.fromList [A.String "hi"]))]]

      -- We shouldn't get hlint output by default
      itHasDisplayDatas lang etaExpandCode []

    describe "LSP" $ do
      haskellDiagnosticsTests lsName

      it "document highlight" $ doNotebookSession documentHighlightCode $ \filename -> do
        ident <- openDoc filename "haskell"
        let desired = [
              DocumentHighlight (Range (Position 0 0) (Position 0 3)) (Just HkWrite)
              , DocumentHighlight (Range (Position 1 9) (Position 1 12)) (Just HkRead)
              ]
        getHighlights ident (Position 0 1) >>= (`shouldBe` List desired)

      describe "hover" $ do
        it "hovers foo" $ doNotebookSession hoverCode $ \filename -> do
          ident <- openDoc filename "haskell"
          hover <- getHoverOrException ident (Position 0 1)
          allHoverText hover `textShouldContain` [i|foo|]
          allHoverText hover `textShouldContain` [i|main.ipynb.hs:1:1|]

        it "hovers putStrLn" $ doNotebookSession hoverCode $ \filename -> do
          ident <- openDoc filename "haskell"
          hover <- getHoverOrException ident (Position 1 1)
          (hover ^. range) `shouldBe` Just (Range (Position 1 0) (Position 1 8))
          let HoverContents (MarkupContent {..}) = hover ^. contents
          _value `textShouldContain` "putStrLn :: String -> IO ()"

      it "symbols" $ doNotebookSession documentHighlightCode $ \filename -> do
        ident <- openDoc filename "haskell"
        Left documentSymbols <- getDocumentSymbols ident
        fmap (^. name) documentSymbols `shouldBe` ["foo"]

      it "code actions" $ doNotebookSession documentHighlightCode $ \filename -> do
        ident <- openDoc filename "haskell"
        actions <- getCodeActions ident (Range (Position 1 0) (Position 1 8))
        actions `shouldBe` []

doNotebookSession :: (
  MonadUnliftIO m, HasNixEnvironment context, HasBaseContext context, MonadBaseControl IO m, MonadThrow m
  ) => Text -> (FilePath -> Session (ExampleT context m) a) -> ExampleT context m a
doNotebookSession = doSession' "main.ipynb"

doSession' :: (
  MonadUnliftIO m, HasNixEnvironment context, HasBaseContext context, MonadBaseControl IO m, MonadThrow m
  ) => Text -> Text -> (FilePath -> Session (ExampleT context m) a) -> ExampleT context m a
doSession' filename code cb = do
  withRunInIO $ \runInIO -> runInIO $ withLspSession lsName (T.unpack filename) documentHighlightCode $ do
    cb (T.unpack filename)

documentHighlightCode :: Text
documentHighlightCode = [__i|foo = "hello"
                             putStrLn foo|]

hoverCode :: Text
hoverCode = [__i|foo = "hello"
                 putStrLn foo
                 import Data.Aeson|]

lsName :: Text
lsName = "haskell-language-server"

kernelSpec lang = NixKernelSpec {
  nixKernelName = lang
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just [i|Haskell (#{lang})|]
  , nixKernelPackages = [nameOnly "aeson", nameOnly "bytestring"]
  , nixKernelLanguageServers = [nameOnly "haskell-language-server"]
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Nothing
  }

kernelSpecWithHlintOutput lang = (kernelSpec lang) {
  nixKernelSettings = Just $ HM.fromList [
      ("enableHlintOutput", A.Bool True)
      ]
  }


main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
