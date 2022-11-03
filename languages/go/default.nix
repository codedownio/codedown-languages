{ pkgs
, callPackage
, lib
, symlinkJoin
, go-langserver
}:

let
  common = callPackage ../common.nix {};

  baseCandidates = [
    "go"
    "go_1_17"
    "go_1_18"
    "go_1_19"
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
      packageOptions = {};
      packageSearch = common.searcher packageOptions;

      languageServerOptions = {
        gopls = callPackage ./language-server-gopls.nix { kernelName = x; };
      };
      languageServerSearch = common.searcher languageServerOptions;

      build = args@{
        packages ? []
        , languageServers ? []
        , attrs ? ["go"]
        , extensions ? ["go"]
        , metaOnly ? false
      }:
        symlinkJoin {
          name = "go";
          paths = [
            (callPackage ./kernel.nix { inherit attrs extensions metaOnly; })
            (callPackage ./mode_info.nix { inherit attrs extensions; })
          ]
          ++ (if metaOnly then [] else [
            go
          ])
          ++ (if metaOnly then [] else (map (y: builtins.getAttr y languageServerOptions) languageServers));

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
