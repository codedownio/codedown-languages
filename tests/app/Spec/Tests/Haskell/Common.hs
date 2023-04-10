{-# LANGUAGE CPP #-}

module Spec.Tests.Haskell.Common where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson as A
import Data.String.Interpolate
import Data.Text as T
import Language.LSP.Test hiding (message)
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.NixTypes
import TestLib.Types (HasNixEnvironment)

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap          as HM
#else
import qualified Data.HashMap.Strict        as HM
#endif

lsName :: Text
lsName = "haskell-language-server"

doNotebookSession :: (
  MonadUnliftIO m, HasNixEnvironment context, HasBaseContext context, MonadBaseControl IO m, MonadThrow m
  ) => Text -> (FilePath -> Session (ExampleT context m) a) -> ExampleT context m a
doNotebookSession = doSession' "main.ipynb"

doSession' :: (
  MonadUnliftIO m, HasNixEnvironment context, HasBaseContext context, MonadBaseControl IO m, MonadThrow m
  ) => Text -> Text -> (FilePath -> Session (ExampleT context m) a) -> ExampleT context m a
doSession' filename code cb = do
  withRunInIO $ \runInIO -> runInIO $ withLspSession lsName (T.unpack filename) code [] $ do
    cb (T.unpack filename)

kernelSpec :: Text -> NixKernelSpec
kernelSpec lang = NixKernelSpec {
  nixKernelName = lang
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just [i|Haskell (#{lang})|]
  , nixKernelPackages = [nameOnly "aeson", nameOnly "bytestring"]
  , nixKernelLanguageServers = [nameOnly "haskell-language-server"]
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettings = Just $ HM.fromList [
      ("haskell-language-server.debug", A.Bool True)
      ]
  }

kernelSpecWithHlintOutput :: Text -> NixKernelSpec
kernelSpecWithHlintOutput lang = (kernelSpec lang) {
  nixKernelSettings = Just $ HM.fromList [
      ("enableHlintOutput", A.Bool True)
      ]
  }
