{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Spellchecker (tests) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Data.String.Interpolate
import Data.Text
import Data.Text.IO as T
import Language.LSP.Protocol.Lens hiding (actions, diagnostics, executeCommand, id)
import Language.LSP.Protocol.Types
import Language.LSP.Test hiding (message)
import Safe
import System.FilePath
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits (waitUntil)
import TestLib.JupyterRunnerContext
import TestLib.LSP
import TestLib.NixEnvironmentContext
import UnliftIO.Directory


otherConfig :: [Text]
otherConfig = [
  "language-servers.spellchecker.enable = true;"
  ]

tests :: TopSpec
tests = describe "Spellchecker" $ introduceNixEnvironment [] otherConfig "Spellchecker env" $ introduceJustBubblewrap $ do
  it "Gets diagnostics and a working code action" $ do
    lspConfig <- findLspConfig "spellchecker"
    (pathToUse, closure) <- getPathAndNixEnvironmentClosure
    withLspSession lspConfig pathToUse closure "test.md" [i|\# This is mispelled|] [] $ \lspHomeDir -> do
      ident <- openDoc "test.md" "spellchecker"
      waitUntil 300.0 $ do
        diagnostics <- waitForDiagnostics
        lift $ assertDiagnosticRanges diagnostics [(Range (Position 0 10) (Position 0 19), Nothing)]

      actions <- getCodeActions ident (Range (Position 0 0) (Position 0 19))
      fmap getTitle actions `shouldContain` ["misspelled"]

      case headMay [x | x <- actions, getTitle x == "Add to dictionary"] of
        Nothing -> expectationFailure [i|Couldn't find "Add to dictionary" action|]
        Just (InL cmd) -> executeCommand cmd
        Just (InR ca) -> executeCodeAction ca

      waitUntil 30 $ do
        let datPath = lspHomeDir </> ".codedown/personal-dictionary.dat"
        doesFileExist datPath >>= (`shouldBe` True)
        liftIO (T.readFile datPath) >>= (`shouldBe` "mispelled\n")

  it "Uses a personal dictionary on startup" $ do
    lspConfig <- findLspConfig "spellchecker"
    (pathToUse, closure) <- getPathAndNixEnvironmentClosure
    let extraFiles = [(".codedown/personal-dictionary.dat", "mispelled\n")]
    withLspSession' id lspConfig pathToUse closure "test.md" [i|\# This is mispelled|] extraFiles $ \_homeDir -> do
      _ident <- openDoc "test.md" "spellchecker"
      waitUntil 300.0 $ do
        diagnostics <- waitForDiagnostics
        lift $ assertDiagnosticRanges diagnostics []

  testDiagnostics "spellchecker" "test.md" Nothing [i|I've done a thing.|] $ \diagnostics -> do
    assertDiagnosticRanges diagnostics []


getTitle :: (HasTitle a Text, HasTitle b Text) => (a |? b) -> Text
getTitle (InL x) = x ^. title
getTitle (InR x) = x ^. title

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
