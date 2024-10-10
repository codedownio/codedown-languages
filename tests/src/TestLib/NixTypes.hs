{-# LANGUAGE TemplateHaskell #-}

module TestLib.NixTypes where

import Data.Aeson as A
import Data.Aeson.TH
import Data.Map
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
  , nameAndSettingsSettings :: Maybe A.Object
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

data ModeInfo = ModeInfo {
  modeInfoAttrs :: [Text]
  , modeInfoCodeMirrorMode :: Maybe Text
  , modeInfoCodeMirrorMimeType :: Maybe Text
  , modeInfoExtensions :: [Text]
  } deriving (Show, Eq, Ord)
deriveJSON toSnake2 ''ModeInfo

data NixMeta = NixMeta {
  nixMetaName :: Maybe Text
  , nixMetaDescription :: Maybe Text
  , nixMetaDisplayName :: Maybe Text
  , nixMetaIcon :: Maybe Text
  , nixMetaCategory :: Maybe Text
  , nixMetaVersion :: Maybe Text

  , nixMetaHomepage :: Maybe Text
  , nixMetaDownloadPage :: Maybe Text
  , nixMetaChangelog :: Maybe Text

  , nixMetaAvailable :: Maybe Bool
  , nixMetaBroken :: Maybe Bool
  , nixMetaUnfree :: Maybe Bool
  , nixMetaUnsupported :: Maybe Bool
  , nixMetaInsecure :: Maybe Bool
  , nixMetaHasPackages :: Maybe Bool
  , nixMetaLessCommon :: Maybe Bool
  , nixMetaMainProgram :: Maybe FilePath
  , nixMetaOutputs :: Maybe [Text]

  , nixMetaMaintainers :: Maybe A.Array

  , nixMetaSpdxId :: Maybe String

  , nixMetaSettingsSchema :: Maybe A.Object
  , nixMetaModes :: Maybe ModeInfo
  , nixMetaLanguageServerNames :: Maybe [Text]
  } deriving (Show, Eq, Ord)
deriveJSON toSnake2 ''NixMeta

data NixPackage = NixPackage {
  nixPackageName :: Text
  , nixPackageSettings :: Maybe (Map Text A.Value)
  , nixPackageMeta :: NixMeta
  , nixPackagePackages :: Maybe [NixPackage]
  }
deriveJSON toSnake2 ''NixPackage

data NixHydrationResult = NixHydrationResult {
  nixHydrationResultPackages :: Map Text NixPackage
  }
deriveJSON toSnake3 ''NixHydrationResult
