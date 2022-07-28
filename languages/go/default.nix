{ pkgs
, callPackage
, lib
, symlinkJoin
}:

let
  common = callPackage ../common.nix {};

  baseCandidates = [
    "go"
  ];

  repls = go: {};

in

with lib;

listToAttrs (map (x:
  let
    go = getAttr x pkgs;

    meta = go.meta // {
      baseName = x;
      displayName = "Go";
      version = go.version;
      icon = ./logo-64x64.png;
    };

  in {
    name = x;
    value = rec {
      packageOptions = getAttr x packagesLookup;
      packageSearch = common.searcher packageOptions;

      languageServerOptions = {};
      languageServerSearch = common.searcher languageServerOptions;

      build = args@{
        packages ? []
        , languageServers ? []
        , attrs ? ["go"]
        , extensions ? ["clj"]
        , metaOnly ? false
      }:
        symlinkJoin {
          name = "go";
          paths = [
            go
            (callPackage ./kernel.nix { inherit attrs extensions; })
            (callPackage ./mode_info.nix { inherit attrs extensions; })
          ];
          passthru = {
            inherit meta packageOptions languageServerOptions;
            args = args // { baseName = x; };
            repls = repls go;
          };
        };

      inherit meta;
    };
  }

) baseCandidates)
