{-# LANGUAGE RankNTypes #-}

module Spec.Tests.Postgres (tests) where

import Control.Lens
import Data.String.Interpolate
import Data.Text
import Language.LSP.Types
import Language.LSP.Types.Lens
import Test.Sandwich as Sandwich
import TestLib.Contexts.PostgresqlData
import TestLib.Contexts.PostgresqlDatabase
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import UnliftIO.Concurrent


kernelSpec = NixKernelSpec {
  nixKernelChannel = "codedown"
  , nixKernelLanguage = "postgres"
  , nixKernelDisplayName = Just "Postgres"
  , nixKernelPackages = []
  , nixKernelLanguageServers = [
      -- nameOnly ""
      ]
  , nixKernelExtraJupyterConfig = Nothing
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelSettingsSchema = Nothing
  , nixKernelSettings = Nothing
  }

tests :: TopSpec
tests = describe "Postgres tests" $ introduceNixEnvironment [kernelSpec] [] "Postgres" $ introduceJupyterRunner $
  introducePostgres Nothing $ introducePostgresData $ do
    it "pauses" $ do
      (_, ctx) <- getContext postgresDb
      info [i|conn string: #{postgresConnString ctx}|]
      threadDelay 99999999999999

    it "selects from test_table" $ do
      (_, ctx) <- getContext postgresDb
      let connStr = postgresConnString ctx
      info [i|Connection string: #{connStr}|]
      testKernelStdout' "postgres" [__i|-- connection: #{connStr}
                                        SELECT * FROM test_table
                                       |] ""

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
