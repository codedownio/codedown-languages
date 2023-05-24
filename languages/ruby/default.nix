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

  settingsSchema = [
    {
      target = "lsp.solargraph.enable";
      title = "Enable Solargraph language server";
      type = "boolean";
      defaultValue = true;
    }
  ];

  chooseLanguageServers = settings: packageOptions: kernelName:
    []
    ++ lib.optionals (common.isTrue settings "lsp.solargraph.enable") [(callPackage ./solargraph.nix { rubyPackages = packageOptions; inherit kernelName; })]
    ;

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
      inherit settingsSchema;
    };

  in {
    name = x;
    value = rec {
      packageOptions = getAttr x packagesLookup;
      packageSearch = common.searcher packageOptions;

      build = args@{
        packages ? []
        , settings ? {}
        , attrs ? [x "ruby"]
        , extensions ? ["rb"]
        , metaOnly ? false
      }:
        let
          settingsToUse = (common.makeDefaultSettings settingsSchema) // settings;
        in symlinkJoin {
          name = x;
          paths = [
            (callPackage ./kernel.nix { inherit attrs extensions; })
            modeInfo
          ]
          ++ (if metaOnly then [] else [ruby])
          ++ (if metaOnly then [] else chooseLanguageServers settingsToUse packageOptions x)
          ;
          passthru = {
            args = args // { baseName = x; };
            inherit meta packageOptions;
          };
        };

      inherit meta;
    };
  }

) (filter (x: (common.hasAttrSafe x pkgs) && !(attrByPath [x "meta" "broken"] false pkgs)) baseCandidates))
