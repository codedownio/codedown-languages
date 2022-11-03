{ lib
, pkgs
, callPackage
, stdenv
, writeTextDir
, symlinkJoin
}:

let
  common = callPackage ../common.nix {};

  baseCandidates = [
    "ruby"
    "ruby_2_0"
    "ruby_2_1_0"
    "ruby_2_2_9"
    "ruby_2_3"
    "ruby_2_3_6"
    "ruby_2_4"
    "ruby_2_4_3"
    "ruby_2_5"
    "ruby_2_5_0"
    "ruby_2_6"
    "ruby_2_7"
    "ruby_3_0"
    "ruby_3_1"
  ];

  packagesLookup = {
    ruby = pkgs.rubyPackages;
    ruby_2_0 = {};
    ruby_2_1_0 = {};
    ruby_2_2_9 = {};
    ruby_2_3 = {};
    ruby_2_3_6 = {};
    ruby_2_4 = pkgs.rubyPackages_2_4;
    ruby_2_4_3 = pkgs.rubyPackages_2_4;
    ruby_2_5 = pkgs.rubyPackages_2_5;
    ruby_2_5_0 = pkgs.rubyPackages_2_5;
    ruby_2_6 = pkgs.rubyPackages_2_6;
    ruby_2_7 = pkgs.rubyPackages_2_7;
    ruby_3_0 = pkgs.rubyPackages_3_0;
    ruby_3_1 = pkgs.rubyPackages_3_1;
  };

  modeInfo = writeTextDir "lib/codedown/modes/ruby.yaml" (pkgs.lib.generators.toYAML {} [{
    attr_name = "ruby";
    code_mirror_mode = "ruby";
    extensions_to_highlight = ["rb"];
    extensions_to_run = ["rb"];
  }]);

in

with lib;

listToAttrs (map (x:
  let
    ruby = getAttr x pkgs;

    meta = ruby.meta // {
      baseName = x;
      displayName = "Ruby " + ruby.version;
      version = ruby.version;
      icon = ./logo-64x64.png;
    };

  in {
    name = x;
    value = rec {
      packageOptions = getAttr x packagesLookup;
      packageSearch = common.searcher packageOptions;

      languageServerOptions = {
        solargraph = callPackage ./solargraph.nix {
          rubyPackages = packageOptions;
          kernelName = x;
        };
      };
      languageServerSearch = common.searcher languageServerOptions;

      build = args@{
        packages ? []
        , languageServers ? []
        , attrs ? [x "ruby"]
        , extensions ? ["rb"]
        , metaOnly ? false
      }:
        symlinkJoin {
          name = x;
          paths = [
            (callPackage ./kernel.nix { inherit attrs extensions; })
            modeInfo
          ]
          ++ (if metaOnly then [] else [
            ruby
          ])
          ++ (if metaOnly then [] else (map (y: builtins.getAttr y languageServerOptions) languageServers))
          ;
          passthru = {
            args = args // { baseName = x; };
            inherit meta packageOptions languageServerOptions;
          };
        };

      inherit meta;
    };
  }

) (filter (x: (common.hasAttrSafe x pkgs) && !(attrByPath [x "meta" "broken"] false pkgs)) baseCandidates))
