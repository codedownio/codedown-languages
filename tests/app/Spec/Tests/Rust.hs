{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Rust (tests) where

import Data.Aeson as A
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Vector as V
import Safe
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.TestSearchers
import TestLib.Types
import TestLib.Util

import qualified Spec.Tests.Rust.Changes as Changes
import qualified Spec.Tests.Rust.Completion as Completion
import qualified Spec.Tests.Rust.Diagnostics as Diagnostics
import qualified Spec.Tests.Rust.Hovers as Hovers


tests :: LanguageSpec
tests = describe "Rust" $ introduceNixEnvironment [kernelSpec] [] "Rust" $ do
  introduceJupyterRunner $ describe "Kernel" $ do
    testKernelSearchersBuild "rust"
    testHasExpectedFields "rust"

    testKernelStdout "rust" [__i|println!("hi")|] "hi\n"

    testKernelStdout "rust" [__i|use serde::{Serialize, Deserialize};

                                 \#[derive(Serialize, Deserialize, Debug)]
                                 struct Point {
                                     x: i32,
                                     y: i32,
                                 }

                                 let point = Point { x: 1, y: 2 };

                                 let serialized: String = serde_json::to_string(&point).unwrap();
                                 println!("serialized = {}", serialized);
                                 |] [i|serialized = {"x":1,"y":2}\n|]
    testKernelStdoutCallback "rust" randCode $ \case
      Just t -> case readMay (T.unpack (T.strip t)) of
        Just (x :: Int) | x >= 0 && x < 256 -> return ()
        _ -> expectationFailure [i|Unexpected output: #{show t}|]
      Nothing -> expectationFailure [i|Kernel produced no output.|]

  describe "LSP" $ do
    Changes.tests
    Completion.tests
    Diagnostics.tests
    Hovers.tests

-- We need a sleep to make this test reliable. It seems the kernel has a problem where
-- it can exit before flushing stdout?
-- See TODO: file issue on evcxr
randCode :: T.Text
randCode = [__i|use rand::prelude::*;
                let x: u8 = random();
                println!("{}", x);|]

kernelSpec :: NixKernelSpec
kernelSpec = NixKernelSpec {
  nixKernelName = "rust"
  , nixKernelChannel = "codedown"
  , nixKernelDisplayName = Just "Rust"
  , nixKernelPackages = [
      nameOnly "rand"

      , NameAndSettings "serde" (Just (aesonFromList [("features", A.Array (V.fromList ["derive"]))]))
      , nameOnly "serde_json"
      , nameOnly "serde_derive"
      ]
  , nixKernelMeta = Nothing
  , nixKernelIcon = Nothing
  , nixKernelExtraConfig = Just [
      "lsp.rust-analyzer.debug = true"
      ]
  }

main :: IO ()
main = jupyterMain tests
