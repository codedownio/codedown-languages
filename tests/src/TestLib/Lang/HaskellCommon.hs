{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestLib.Lang.HaskellCommon (haskellCommonTests) where

import Control.Lens ((^.))
import Control.Monad.Catch (MonadThrow, onException)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson as A
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Vector as V
import Language.LSP.Test
import Language.LSP.Types
import Language.LSP.Types.Lens
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.JupyterTypes
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.Types (HasNixEnvironment)
import UnliftIO.Concurrent


haskellCommonTests :: Text -> TopSpec
haskellCommonTests lang = describe [i|Haskell #{lang}|] $ introduceNixEnvironment [kernelSpec lang] [] "Haskell" $ introduceJupyterRunner $ do
  testNotebookDisplayDataOutputs lang [__i|putStrLn "hi"|] [M.fromList [(MimeType "text/plain", A.Array (V.fromList [A.String "hi"]))]]

  testDiagnostics lsName "Foo.hs" [__i|module Foo where
                                       foo = bar
                                      |] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics [(Range (Position 1 6) (Position 1 9), Just (InR "-Wdeferred-out-of-scope-variables"))]

  testDiagnostics lsName "main.ipynb" [__i|-- Some comment
                                           foo = bar

                                           putStrLn "HI"
                                          |] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics [(Range (Position 1 6) (Position 1 9), Just (InR "-Wdeferred-out-of-scope-variables"))]

  it "does document highlight" $ doSession documentHighlightCode $ \filename -> do
    ident <- openDoc filename "haskell"
    let desired = [
          DocumentHighlight (Range (Position 0 0) (Position 0 3)) (Just HkWrite)
          , DocumentHighlight (Range (Position 1 9) (Position 1 12)) (Just HkRead)
          ]
    getHighlights ident (Position 0 1) >>= (`shouldBe` List desired)

  it "does hover" $ doSession documentHighlightCode $ \filename -> do
    ident <- openDoc filename "haskell"
    Just hover <- getHover ident (Position 1 1)
    (hover ^. range) `shouldBe` (Just (Range (Position 1 0) (Position 1 8)))
    let HoverContents (MarkupContent {..}) = hover ^. contents
    _value `textShouldContain` "putStrLn :: String -> IO ()"


doSession :: (
  MonadUnliftIO m, HasNixEnvironment context, HasBaseContext context, MonadBaseControl IO m, MonadThrow m
  ) => Text -> (FilePath -> Session ()) -> ExampleT context m ()
doSession code cb = do
  let filename :: Text = "main.ipynb"
  withRunInIO $ \runInIO -> runInIO $ withLspSession lsName (T.unpack filename) documentHighlightCode $ do
    cb (T.unpack filename)

documentHighlightCode = [__i|foo = "hello"
                             putStrLn foo|]

lsName :: Text
lsName = "haskell-language-server"

kernelSpec lang = NixKernelSpec {
  nixKernelChannel = "codedown"
  , nixKernelLanguage = lang
  , nixKernelDisplayName = Just "Haskell"
  , nixKernelPackages = []
  , nixKernelLanguageServers = [nameOnly "haskell-language-server"]
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettingsSchema = Nothing
  , nixKernelSettings = Nothing
  }
