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
    "clojure"
  ];

  packagesLookup = {
    clojure = {};
  };

  repls = clojure: {
    clojure = {
      display_name = "Clojure " + clojure.version;
      attr = "clojure";
      args = ["${clojure}/bin/clojure"];
      icon = ./clojure-logo-64x64.png;
    };
  };

  settingsSchema = [
    {
      target = "lsp.clojure-lsp.enable";
      title = "Enable clojure-lsp language server";
      type = "boolean";
      defaultValue = true;
    }
  ];

  clojure-lsp = callPackage ./clojure-lsp.nix {};

  chooseLanguageServers = settings: kernelName:
    []
    ++ lib.optionals (common.isTrue settings "lsp.clojure-lsp.enable") [(callPackage ./language-server.nix { inherit clojure-lsp kernelName; })]
  ;

in

with lib;

listToAttrs (map (x:
  let
    clojure = getAttr x pkgs;

    meta = clojure.meta // {
      baseName = x;
      displayName = "Clojure";
      version = clojure.version;
      icon = ./clojure-logo-64x64.png;
      inherit settingsSchema;
    };

    packageOptions = getAttr x packagesLookup;
    packageSearch = common.searcher packageOptions;
    versions = {
      clojure = clojure.version;
      clojure-lsp = clojure-lsp.version;
    };

  in {
    name = x;
    value = lib.makeOverridable (args@{
      packages ? []
      , settings ? {}
      , attrs ? ["clojure"]
      , extensions ? ["clj"]
    }:
      let
        settingsToUse = (common.makeDefaultSettings settingsSchema) // settings;
        languageServers = chooseLanguageServers settingsToUse x;
      in symlinkJoin {
        name = "clojure";
        paths = [
          (callPackage ./kernel.nix { inherit attrs extensions version; })
          clojure
        ]
        ++ languageServers
        ;

        passthru = {
          inherit meta packageOptions packageSearch versions;
          inherit settingsSchema settings;
          args = args // { baseName = x; };
          repls = repls clojure;
          modes = {
            inherit attrs extensions;
            code_mirror_mode = "clojure";
          };
          languageServerNames = map (x: x.languageServerName) languageServers;
        };
      }
    ) {}
    ;
  }

) (filter (x: (common.hasAttrSafe x pkgs) && !(attrByPath [x "meta" "broken"] false pkgs)) baseCandidates))
