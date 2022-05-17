{-# LANGUAGE TemplateHaskell #-}

module TestLib.Types where

import Data.Aeson as A
import Data.Aeson.TH
import qualified Data.List as L
import Data.Text
import Test.Sandwich
import TestLib.Aeson
import TestLib.NixTypes


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
  , nixSrcSha256 = lockedNarHash
  }

nixEnvironment :: Label "nixEnvironment" FilePath
nixEnvironment = Label
type HasNixEnvironment context = HasLabel context "nixEnvironment" FilePath

jupyterRunner :: Label "jupyterRunner" FilePath
jupyterRunner = Label
type HasJupyterRunner context = HasLabel context "jupyterRunner" FilePath
