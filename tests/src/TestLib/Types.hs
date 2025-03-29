{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}

module TestLib.Types where

import Data.Aeson as A
import Data.Aeson.TH
import qualified Data.List as L
import Data.Text
import Options.Applicative hiding (action)
import Test.Sandwich as Sandwich
import TestLib.Aeson
import TestLib.NixTypes


-- * CLI

data SpecialOptions = SpecialOptions {
  optTestParallelism :: Int
  }

specialOptions :: Parser SpecialOptions
specialOptions = SpecialOptions
  <$> option auto (long "test-parallelism" <> short 'n' <> showDefault <> help "Test parallelism" <> value 4 <> metavar "INT")

-- * Nix

data LockedType = LockedTypeGithub
  deriving (Show)
deriveJSON toSnakeC1 ''LockedType

data Locked = LockedGithub {
  lockedNarHash :: Text
  , lockedOwner :: Text
  , lockedRepo :: Text
  , lockedRev :: Text
  } deriving (Show)
deriveJSON ((dropNAndToCamelCaseOptions (L.length ("locked" :: String))) {
               A.tagSingleConstructors = True
               , A.constructorTagModifier = toSnakeAndDropFirstWord
               , A.sumEncoding = A.defaultTaggedObject { A.tagFieldName = "type" }
               }) ''Locked

lockedToNixSrcSpec :: Text -> Locked -> NixSrcSpec
lockedToNixSrcSpec name (LockedGithub {..}) = NixSrcFetchFromGithub {
  nixSrcName = name
  , nixSrcOwner = lockedOwner
  , nixSrcRepo = lockedRepo
  , nixSrcRev = lockedRev
  , nixSrcHash = lockedNarHash
  }

-- * Labels

nixEnvironment :: Label "nixEnvironment" FilePath
nixEnvironment = Label
type HasNixEnvironment context = HasLabel context "nixEnvironment" FilePath

jupyterRunner :: Label "jupyterRunner" FilePath
jupyterRunner = Label
type HasJupyterRunner context = HasLabel context "jupyterRunner" FilePath

maybeBubblewrap :: Label "maybeBubblewrap" (Maybe FilePath)
maybeBubblewrap = Label
type HasMaybeBubblewrap context = HasLabel context "maybeBubblewrap" (Maybe FilePath)

bootstrapNixpkgs :: Label "bootstrapNixpkgs" FilePath
bootstrapNixpkgs = Label
type HasBootstrapNixpkgs context = HasLabel context "bootstrapNixpkgs" FilePath

-- * Spec types

type SomeLanguageSpec context = (
  HasBaseContext context
  , HasJupyterRunner context
  , HasMaybeBubblewrap context
  , HasBootstrapNixpkgs context
  )

type LanguageSpec = forall context. SomeLanguageSpec context => SpecFree context IO ()

type SimpleSpec = forall context. (HasBaseContext context, HasBootstrapNixpkgs context) => SpecFree context IO ()
