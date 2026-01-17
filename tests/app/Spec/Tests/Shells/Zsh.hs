{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Shells.Zsh (tests) where

import Data.String.Interpolate
import Data.Text
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext (introduceTargetSystem)
import TestLib.NixEnvironmentContext
import TestLib.Types


otherConfig :: [Text]
otherConfig = [
  "shells.zsh.enable = true;"
  ]

tests :: NixEnvSpec
tests = describe "ZSH" $ introduceNixEnvironment [] otherConfig "ZSH environment" $ do
  it "can run a command" $ do
    nixEnv <- getContext nixEnvironment
    info [i|Got nixEnv: #{nixEnv}|]

    pending

main :: IO ()
main = runSandwichWithCommandLineArgs' Sandwich.defaultOptions specialOptions $
  introduceTargetSystem tests
