{ lib
, pkgs
, callPackage
, stdenv
, writeTextDir
, symlinkJoin
}:

let
  common = callPackage ../common.nix {};

  filterFn = x: (common.hasAttrSafe x pkgs) && !(lib.attrByPath [x "meta" "broken"] false pkgs);

  baseCandidates = lib.filter filterFn [
    # "ruby_2_4"
    # "ruby_2_4_3"
    # "ruby_2_5"
    # "ruby_2_5_0"
    # "ruby_2_6"
    # "ruby_2_7"

    "ruby"
    "ruby_3_0"
    "ruby_3_1"
    "ruby_3_2"
    "ruby_3_3"
  ];

  packagesLookup = lib.filterAttrs (k: v: filterFn k) {
    # ruby_2_4 = pkgs.rubyPackages_2_4;
    # ruby_2_4_3 = pkgs.rubyPackages_2_4;
    # ruby_2_5 = pkgs.rubyPackages_2_5;
    # ruby_2_5_0 = pkgs.rubyPackages_2_5;
    # ruby_2_6 = pkgs.rubyPackages_2_6;
    # ruby_2_7 = pkgs.rubyPackages_2_7;

    ruby = pkgs.rubyPackages;
    ruby_3_0 = pkgs.rubyPackages_3_0;
    ruby_3_1 = pkgs.rubyPackages_3_1;
    ruby_3_2 = pkgs.rubyPackages_3_2;
    ruby_3_3 = pkgs.rubyPackages_3_3;
  };

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
      displayName = "Ruby";
      version = ruby.version;
      icon = ./iruby-64x64.png;
      inherit settingsSchema;
    };

  in {
    name = x;
    value = rec {
      packageOptions = getAttr x packagesLookup;
      packageSearch = common.searcher packageOptions;
      versions = {
        ruby = builtins.toString ruby.version;
        solargraph = packageOptions.solargraph.version;
      };

      build = args@{
        packages ? []
        , settings ? {}
        , attrs ? [x "ruby"]
        , extensions ? ["rb"]
      }:
        let
          settingsToUse = (common.makeDefaultSettings settingsSchema) // settings;
        in symlinkJoin {
          name = x;
          paths = [
            (callPackage ./kernel.nix {
              iruby = (callPackage ./iruby { inherit ruby; }).iruby;
              inherit attrs extensions version;
            })
            ruby
          ]
          ++ (chooseLanguageServers settingsToUse packageOptions x)
          ;
          passthru = {
            inherit meta packageOptions;
            inherit settingsSchema settings;
            args = args // { baseName = x; };
            modes = {
              inherit attrs extensions;
              code_mirror_mode = "ruby";
            };
          };
        };
    };
  }

) baseCandidates)
