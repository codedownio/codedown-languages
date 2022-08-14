{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestLib.Lang.HaskellCommon (haskellCommonTests) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Unlift
import Data.Aeson as A
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Vector as V
import Language.LSP.Test
import Language.LSP.Types
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.JupyterTypes
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes


haskellCommonTests :: Text -> TopSpec
haskellCommonTests lang = describe [i|Haskell #{lang}|] $ introduceNixEnvironment [kernelSpec lang] [] "Haskell" $ introduceJupyterRunner $ do
  testNotebookDisplayDataOutputs lang [__i|putStrLn "hi"|] [M.fromList [(MimeType "text/plain", A.Array (V.fromList [A.String "hi"]))]]

  testDiagnostics "haskell-language-server" "Foo.hs" [__i|module Foo where
                                                          foo = bar
                                                          |] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics [(Range (Position 1 6) (Position 1 9), Just (InR "-Wdeferred-out-of-scope-variables"))]

  testDiagnostics "haskell-language-server" "main.ipynb" [__i|-- Some comment
                                                              foo = bar

                                                              putStrLn "HI"
                                                              |] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics [(Range (Position 1 6) (Position 1 9), Just (InR "-Wdeferred-out-of-scope-variables"))]

  it "does document highlight" $ do
    let filename :: Text = "haskell-language-server"
    let name :: Text = "main.ipynb"
    withRunInIO $ \runInIO -> runInIO $ withLspSession filename (T.unpack name) documentHighlightCode $ do
      ident <- openDoc (T.unpack filename) name
      getHighlights ident (Position 0 1) >>= (`shouldBe` (List []))


documentHighlightCode = [__i|foo = "hello"
                             putStrLn foo|]


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
