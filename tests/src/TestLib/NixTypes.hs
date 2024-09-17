{-# LANGUAGE TemplateHaskell #-}

module TestLib.NixTypes where

import Data.Aeson as A
import Data.Aeson.TH
import Data.Text
import TestLib.Aeson


data NixSrcSpec = NixSrcFetchGit {
  nixSrcName :: Text
  , nixSrcUrl :: Text
  , nixSrcRev :: Text
  , nixSrcBranchName :: Maybe Text
  , nixSrcSha256 :: Text
  }
  | NixSrcFetchFromGithub {
      nixSrcName :: Text
      , nixSrcOwner :: Text
      , nixSrcRepo :: Text
      , nixSrcRev :: Text
      , nixSrcSha256 :: Text
      }
  | NixSrcPath {
      nixSrcName :: Text
      , nixSrcPath :: Text
    }
  deriving (Show, Eq, Ord)
deriveJSON toSnakeBoth2 ''NixSrcSpec

data NameAndSettings = NameAndSettings {
  nameAndSettingsName :: Text
  , nameAndSettingsSettings :: Maybe Value
  } deriving (Show, Eq, Ord)
deriveJSON toSnake3 ''NameAndSettings

nameOnly :: Text -> NameAndSettings
nameOnly name = NameAndSettings name Nothing

data ChannelAndAttr = ChannelAndAttr {
  channelAndAttrChannel :: Text
  , channelAndAttrAttr :: Text
  , channelAndAttrName :: Maybe Text
  , channelAndAttrMeta :: Maybe Value
  } deriving (Show, Eq, Ord)
deriveJSON toSnake3 ''ChannelAndAttr

channelAndAttr :: Text -> Text -> ChannelAndAttr
channelAndAttr channel attr = ChannelAndAttr channel attr Nothing Nothing

data NixKernelSpec = NixKernelSpec {
  nixKernelName :: Text
  , nixKernelChannel :: Text
  , nixKernelDisplayName :: Maybe Text
  , nixKernelPackages :: [NameAndSettings]
  , nixKernelMeta :: Maybe Value
  , nixKernelIcon :: Maybe Text
  , nixKernelExtraConfig :: Maybe [Text]
  } deriving (Show, Eq, Ord)
deriveJSON toSnake2 ''NixKernelSpec

data NixEnvironment = NixEnvironment {
  nixEnvironmentChannels :: [NixSrcSpec]
  , nixEnvironmentKernels :: [NixKernelSpec]
  , nixEnvironmentOtherConfig :: [Text]
  } deriving (Show, Eq, Ord)
deriveJSON toSnake2 ''NixEnvironment
