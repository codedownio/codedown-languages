{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with maybe" #-}

module TestLib.NixRendering where

import Data.Aeson as A
import Data.Maybe
import Data.String.Interpolate
import Data.Text
import qualified Data.Text as T
import qualified Data.Vector as V
import TestLib.Aeson
import TestLib.NixTypes

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key             as A
import qualified Data.Aeson.KeyMap          as HM
#else
import qualified Data.HashMap.Strict        as HM
#endif


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

  channels = rec {
#{T.intercalate "\n\n" [indentTo 4 $ renderChannel x | x <- nixEnvironmentChannels]}
  };

  importedChannels = lib.mapAttrs (name: value: let imported = import (channelSpecToChannel name value); in
    if (builtins.isFunction imported) then bootstrapNixpkgs.callPackage imported {}
    else imported
  ) channels;

in

importedChannels.codedown.mkCodeDownEnvironment {
  inherit channels;

  kernels = [
#{T.intercalate "\n" [indentTo 4 $ renderKernel x | x <- nixEnvironmentKernels]}
  ];

  otherPackages = [
#{T.intercalate "\n" [indentTo 4 $ renderOtherPackage x | x <- nixEnvironmentOtherPackages]}
  ];
}
|]

renderChannel :: NixSrcSpec -> Text
renderChannel nixSrcSpec = [i|#{nixSrcName nixSrcSpec} = (#{renderNixSrcSpec nixSrcSpec});|]

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

#if MIN_VERSION_aeson(2,0,0)
    toLine :: (A.Key, A.Value) -> Maybe Text
    toLine (A.toText -> "name", _) = Nothing
    toLine (A.toText -> k, v) = Just [i|  #{snakeToCamelCase $ T.unpack k} = #{showValue v};|]
#else
    toLine :: (Text, A.Value) -> Maybe Text
    toLine ("name", _) = Nothing
    toLine (k, v) = Just [i|  #{snakeToCamelCase $ T.unpack k} = #{showValue v};|]
#endif

    quote t = [i|''#{t}''|]

renderOtherPackage :: ChannelAndAttr -> Text
renderOtherPackage (ChannelAndAttr {..}) = [i|{ channel = "#{channelAndAttrChannel}"; attr = "#{channelAndAttrAttr}"; contents = importedChannels.#{channelAndAttrChannel}.#{channelAndAttrAttr};  }|]

renderKernel :: NixKernelSpec -> Text
renderKernel (NixKernelSpec {..}) = [i|({
  name = "#{nixKernelName}";
  channel = "#{nixKernelChannel}";
  args = {
    packages = [#{T.unwords $ fmap quote $ fmap nameAndMetaName nixKernelPackages}];#{settings}
  };
})|]
  where
    quote x = "\"" <> x <> "\""

    settings = maybe "" (("\n" <>) . indentTo 4 . (\x -> [i|settings = #{x};|]) . aesonToNix . A.Object) nixKernelSettings

aesonToNix :: A.Value -> Text
aesonToNix (A.Bool True) = "true"
aesonToNix (A.Bool False) = "false"
aesonToNix (A.Array xs) = "[" <> T.intercalate " " (fmap aesonToNix (V.toList xs)) <> "]"
aesonToNix (A.String s) = [i|''#{s}''|]
aesonToNix (A.Number n) = [i|#{n}|]
aesonToNix A.Null = [i|null|]
aesonToNix (A.Object os) = let
  ls = [[i|#{k} = #{aesonToNix v};|] | (k, v) <- HM.toList os]
    in [__i|{
              #{T.intercalate "\n" ls}
            }|]

indentTo :: Int -> Text -> Text
indentTo n = T.intercalate "\n" . fmap (space <>) . T.splitOn "\n"
  where space = T.replicate n " "
