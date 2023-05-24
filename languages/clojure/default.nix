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
      args = ["${clojure}/bin/clojure"];
      icon = ./logo-64x64.png;
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

  chooseLanguageServers = settings: kernelName:
  []
  ++ lib.optionals (common.isTrue settings "lsp.clojure-lsp.enable") [(callPackage ./language-server.nix { inherit kernelName; })]
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
      icon = ./logo-64x64.png;
    };

  in {
    name = x;
    value = rec {
      packageOptions = getAttr x packagesLookup;
      packageSearch = common.searcher packageOptions;

      build = args@{
        packages ? []
        , settings ? {}
        , attrs ? ["clojure"]
        , extensions ? ["clj"]
        , metaOnly ? false
      }:
        let
          settingsToUse = (common.makeDefaultSettings settingsSchema) // settings;
        in symlinkJoin {
          name = "clojure";
          paths = [
            (callPackage ./kernel.nix { inherit attrs extensions; })
            (callPackage ./mode_info.nix { inherit attrs extensions; })
          ]
          ++ (if metaOnly then [] else [clojure])
          ++ (if metaOnly then [] else chooseLanguageServers settingsToUse x)
          ;

          passthru = {
            inherit meta packageOptions;
            args = args // { baseName = x; };
            repls = repls clojure;
          };
        };

      inherit meta;
    };
  }

) (filter (x: (common.hasAttrSafe x pkgs) && !(attrByPath [x "meta" "broken"] false pkgs)) baseCandidates))
