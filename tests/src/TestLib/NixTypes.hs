{-# LANGUAGE TemplateHaskell #-}

module TestLib.NixTypes where

import Data.Aeson as A
import Data.Aeson.TH
import Data.Text
import Test.Sandwich
import TestLib.Aeson


data NixSrcType = NixSrcTypeNixpkgs
                | NixSrcTypeFlake
                | NixSrcTypeNormal
  deriving (Show, Eq, Ord)
deriveJSON toSnakeC3 ''NixSrcType

data NixSrcSpec = NixSrcFetchGit {
  nixSrcName :: Text
  , nixSrcUrl :: Text
  , nixSrcRev :: Text
  , nixSrcBranchName :: Maybe Text
  , nixSrcSha256 :: Text
  , nixSrcType :: NixSrcType
  }
  | NixSrcFetchFromGithub {
      nixSrcName :: Text
      , nixSrcOwner :: Text
      , nixSrcRepo :: Text
      , nixSrcRev :: Text
      , nixSrcSha256 :: Text
      , nixSrcType :: NixSrcType
      }
  | NixSrcPath {
      nixSrcName :: Text
      , nixSrcPath :: Text
      , nixSrcType :: NixSrcType
    }
  deriving (Show, Eq, Ord)
deriveJSON toSnakeBoth2 ''NixSrcSpec

data NameAndMeta = NameAndMeta {
  nameAndMetaName :: Text
  , nameAndMetaMeta :: Maybe Value
  } deriving (Show, Eq, Ord)
deriveJSON toSnake3 ''NameAndMeta

nameOnly :: Text -> NameAndMeta
nameOnly name = NameAndMeta name Nothing

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
  nixKernelChannel :: Text
  , nixKernelLanguage :: Text
  , nixKernelDisplayName :: Maybe Text
  , nixKernelPackages :: [NameAndMeta]
  , nixKernelLanguageServers :: [NameAndMeta]
  , nixKernelExtraJupyterConfig :: Maybe Text
  , nixKernelMeta :: Maybe Value
  , nixKernelIcon :: Maybe Text
  , nixKernelSettingsSchema :: Maybe [A.Value]
  , nixKernelSettings :: Maybe A.Value
  } deriving (Show, Eq, Ord)
deriveJSON toSnake2 ''NixKernelSpec

data NixEnvironment = NixEnvironment {
  nixEnvironmentMetaOnly :: Maybe Bool
  , nixEnvironmentChannels :: [NixSrcSpec]
  , nixEnvironmentOverlays :: [NixSrcSpec]
  , nixEnvironmentKernels :: [NixKernelSpec]
  , nixEnvironmentOtherPackages :: [ChannelAndAttr]
  } deriving (Show, Eq, Ord)
deriveJSON toSnake2 ''NixEnvironment
