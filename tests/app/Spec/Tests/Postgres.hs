{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Postgres (tests) where

import Data.Aeson as A
import qualified Data.Map as M
import Data.String.Interpolate
import qualified Data.Vector as V
import Test.Sandwich as Sandwich
import Test.Sandwich.Contexts.Nix (introduceNixContext, nixpkgsReleaseDefault)
import Test.Sandwich.Contexts.PostgreSQL
import TestLib.Contexts.PostgresqlData
import TestLib.Contexts.PostgresqlData ()
import TestLib.JupyterRunnerContext
import TestLib.JupyterTypes
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types


kernelSpec :: NixKernelSpec
kernelSpec = NixKernelSpec {
  nixKernelName = "postgres"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "Postgres"
  , nixKernelPackages = []
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelExtraConfig = Nothing
  }

tests :: LanguageSpec
tests = describe "Postgres tests" $ introduceNixEnvironment [kernelSpec] [] "Postgres" $ introduceJupyterRunner $ do
  testKernelSearchersBuild "postgres"
  testHasExpectedFields "postgres"

  introduceNixContext nixpkgsReleaseDefault $ introducePostgresViaNix defaultPostgresNixOptions $ introducePostgresData $ do
    it "selects from test_table" $ do
      PostgresContext {..} <- getContext postgres
      let connStr = postgresConnString
      info [i|Connection string: #{connStr}|]
      displayDatasShouldSatisfy "postgres" [__i|-- connection: #{connStr}
                                                SELECT * FROM test_table
                                               |] $ \case
        [M.lookup (MimeType "text/plain") -> Just x] ->
          x `shouldBe` (Array (V.fromList [
              String "name      num\n"
            , String "------  -----\n"
            , String "foo         1\n"
            , String "bar         2\n"
            , String "baz         3"
            ]))

        v -> expectationFailure [i|Expected single map with text/plain. Got: #{v}|]

main :: IO ()
main = jupyterMain tests
