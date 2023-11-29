{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Shells.Zsh (tests) where

import Data.String.Interpolate
import Test.Sandwich as Sandwich
import TestLib.NixEnvironmentContext
import TestLib.NixTypes
import TestLib.Types


otherPackages :: [ChannelAndAttr]
otherPackages = [
  channelAndAttr "codedown" "shells.zsh"
  ]

tests :: TopSpec
tests = describe "ZSH" $ introduceNixEnvironment [] otherPackages "ZSH environment" $ do
  it "can run a command" $ do
    nixEnv <- getContext nixEnvironment
    info [i|Got nixEnv: #{nixEnv}|]

    pending

main :: IO ()
main = runSandwichWithCommandLineArgs Sandwich.defaultOptions tests
