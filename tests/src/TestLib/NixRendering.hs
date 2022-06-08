
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


renderNixEnvironmentFlake (NixEnvironment {..}) = [i|\# Autogenerated, please don't edit by hand
{
  description = "Environment";

  inputs.flake-utils.url = "github:numtide/flake-utils";

#{T.intercalate "\n" [indentTo 2 $ renderInput x | x <- nixEnvironmentChannels]}

  outputs = { self, flake-utils, #{T.intercalate ", " (fmap nixSrcName nixEnvironmentChannels)} }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlaysMap = {
#{T.intercalate "\n" [indentTo 10 $ renderChannel x | x <- nixEnvironmentOverlays]}
        };
        overlays = map (x: overlaysMap.${x}) (builtins.attrNames overlaysMap);

        channels = {
#{T.intercalate "\n" [indentTo 10 $ renderChannel x | x <- nixEnvironmentChannels]}
        };

#{T.intercalate "\n" [indentTo 8 $ renderImportedInput x | x <- nixEnvironmentChannels]}

        mkCodeDownEnvironment = codedown-imported.mkCodeDownEnvironment;
      in
        {
          packages = {
            default = mkCodeDownEnvironment {
              inherit channels;
              overlays = overlaysMap;

              kernels = [
#{T.intercalate "\n" [indentTo 16 $ renderKernel x | x <- nixEnvironmentKernels]}
              ];

              otherPackages = [
#{T.intercalate "\n" [indentTo 16 $ renderOtherPackageFlake x | x <- nixEnvironmentOtherPackages]}
              ];

              metaOnly = false;
            };
          };
        }
    );
}
|]
  where
    renderInput (NixSrcSpec {..}) = [i|inputs.#{nixSrcName}.url = "#{nixSrcUrl}";|]

    renderImportedInput src = case nixSrcType src of
      NixSrcTypeNixpkgs -> [i|#{nixSrcName src}-imported = import #{nixSrcName src} { inherit system overlays; };|]
      NixSrcTypeFlake -> [i|#{nixSrcName src}-imported = #{nixSrcName src}.outputs.packages."${system}";|]
      NixSrcTypeNormal -> [i|#{nixSrcName src}-imported = nixSrcName src;|]

    renderOtherPackageFlake :: ChannelAndAttr -> Text
    renderOtherPackageFlake (ChannelAndAttr {..}) = [i|{ channel = "#{channelAndAttrChannel}"; attr = "#{channelAndAttrAttr}"; contents = #{channelAndAttrChannel}-imported.#{channelAndAttrAttr};  }|]

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
