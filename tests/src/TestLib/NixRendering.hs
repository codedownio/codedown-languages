
module TestLib.NixRendering where

import Data.Aeson as A
import Data.Aeson.TH
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.String.Interpolate
import Data.Text
import qualified Data.Text as T
import Test.Sandwich
import TestLib.Aeson
import TestLib.NixTypes


renderNixEnvironment :: FilePath -> NixEnvironment -> Text
renderNixEnvironment bootstrapNixpkgs (NixEnvironment {..}) = [i|\# Autogenerated, please don't edit by hand

let
  bootstrapNixpkgs = import (#{bootstrapNixpkgs}) {};
  fetchgit = bootstrapNixpkgs.fetchgit;
  fetchFromGitHub = bootstrapNixpkgs.fetchFromGitHub;
  runCommand = bootstrapNixpkgs.runCommand;
  lib = bootstrapNixpkgs.lib;

  channelSpecToChannel = name: channel:
    if (channel.tag == "fetch_from_github") then fetchFromGitHub ((removeAttrs channel ["tag" "name"]))
    else if (channel.tag == "fetch_git") then fetchgit (removeAttrs channel ["tag" "name"])
    else if (channel.tag == "path") then channel.path else null;

  overlays = {
#{T.intercalate "\n\n" [indentTo 4 $ renderChannel x | x <- nixEnvironmentOverlays]}  };

  channels = rec {
#{T.intercalate "\n\n" [indentTo 4 $ renderChannel x | x <- nixEnvironmentChannels]}  };

  importedOverlays = lib.mapAttrsToList (name: value: import (channelSpecToChannel name value)) overlays;
  importedChannels = lib.mapAttrs (name: value: import (channelSpecToChannel name value) { overlays = importedOverlays; }) channels;

in

importedChannels.nixpkgs.codedown.mkCodeDownEnvironment {
  inherit channels importedChannels overlays;

  metaOnly = #{A.encode $ fromMaybe False nixEnvironmentMetaOnly};

  kernels = [
#{T.intercalate "\n" [indentTo 4 $ renderKernel x | x <- nixEnvironmentKernels]}
  ];

  otherPackages = [
#{T.intercalate "\n" [indentTo 4 $ renderOtherPackage x | x <- nixEnvironmentOtherPackages]}
  ];
}
|]

renderChannel :: NixSrcSpec -> Text
renderChannel nixSrcSpec = [i|#{nixSrcName nixSrcSpec} = (#{renderNixSrcSpec nixSrcSpec});
|]

renderNixSrcSpec :: NixSrcSpec -> Text
renderNixSrcSpec nixSrcSpec = "{\n" <> (T.intercalate "\n" $ mapMaybe toLine keysAndValues) <> "\n}"
  where
    keysAndValues = case A.toJSON nixSrcSpec of
      A.Object hm -> HM.toList hm
      _ -> []

    showValue :: A.Value -> Text
    showValue (A.String t) = quote t
    showValue (A.Number n) = quote $ T.pack $ show n
    showValue A.Null = "null"
    showValue _ = [i|(throw "Couldn't render expression in NixEnvironment.hs.")|]

    toLine :: (Text, A.Value) -> Maybe Text
    toLine ("name", _) = Nothing
    toLine (k, v) = Just [i|  #{snakeToCamelCase $ T.unpack k} = #{showValue v};|]

    quote t = [i|''#{t}''|]

renderOtherPackage :: ChannelAndAttr -> Text
renderOtherPackage (ChannelAndAttr {..}) = [i|{ channel = "#{channelAndAttrChannel}"; attr = "#{channelAndAttrAttr}"; contents = importedChannels.#{channelAndAttrChannel}.#{channelAndAttrAttr};  }|]

renderKernel :: NixKernelSpec -> Text
renderKernel (NixKernelSpec {..}) = [i|({
  channel = "#{nixKernelChannel}";
  language = "#{nixKernelLanguage}";
  args = {
    packages = [#{T.unwords $ fmap quote $ fmap nameAndMetaName nixKernelPackages}];
    languageServers = [#{T.unwords $ fmap quote $ fmap nameAndMetaName nixKernelLanguageServers}];
  };
})|]
  where quote x = "\"" <> x <> "\""

indentTo :: Int -> Text -> Text
indentTo n = T.intercalate "\n" . fmap (space <>) . T.splitOn "\n"
  where space = T.replicate n " "
