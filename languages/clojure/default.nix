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

  clojure-lsp = (builtins.getFlake "github:clojure-lsp/clojure-lsp/5e3584014f2ac9c13a877dfd7984383346d81609").packages.x86_64-linux.default;

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

  in {
    name = x;
    value = rec {
      packageOptions = getAttr x packagesLookup;
      packageSearch = common.searcher packageOptions;
      versions = {
        clojure = clojure.version;
        clojure-lsp = clojure-lsp.version;
      };

      build = args@{
        packages ? []
        , settings ? {}
        , attrs ? ["clojure"]
        , extensions ? ["clj"]
      }:
        let
          settingsToUse = (common.makeDefaultSettings settingsSchema) // settings;
        in symlinkJoin {
          name = "clojure";
          paths = [
            (callPackage ./kernel.nix { inherit attrs extensions version; })
            clojure
          ]
          ++ (chooseLanguageServers settingsToUse x)
          ;

          passthru = {
            inherit meta packageOptions;
            args = args // { baseName = x; };
            repls = repls clojure;
            modes = {
              inherit attrs extensions;
              code_mirror_mode = "clojure";
            };
          };
        };

      inherit meta;
    };
  }

) (filter (x: (common.hasAttrSafe x pkgs) && !(attrByPath [x "meta" "broken"] false pkgs)) baseCandidates))
