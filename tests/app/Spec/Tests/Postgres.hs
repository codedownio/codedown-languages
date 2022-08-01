
module Spec.Tests.Postgres (tests) where

import Control.Lens
import Data.String.Interpolate
import Language.LSP.Types
import Language.LSP.Types.Lens
import Test.Sandwich as Sandwich
import TestLib.Contexts.PostgresqlDatabase
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import TestLib.NixTypes


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
tests = describe "Postgres" $ introduceNixEnvironment [kernelSpec] [] "Postgres" $ introduceJupyterRunner $ introducePostgres $ do
  testKernelStdout "postgres" [i|SELECT * FROM FOO|] ""

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
