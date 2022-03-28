{ pkgs
, lib
, callPackage
, writeTextDir
, graphviz
, symlinkJoin
}:

let
  common = callPackage ../common.nix {};

  baseCandidates = [
    "graphviz"
  ];

in

lib.listToAttrs (map (x:
  let
    graphviz = lib.getAttr x pkgs;

    meta = graphviz.meta // {
      baseName = "cpp11";
      displayName = "Graphviz " + graphviz.version;
      version = graphviz.version;
      icon = ./logo-64x64.png;
    };

  in {
    name = x;
    value = rec {
      packageOptions = {};
      packageSearch = common.searcher {};

      languageServerOptions = {};
      languageServerSearch = common.searcher languageServerOptions;

      build = args@{
        packages ? []
        , languageServers ? []
        , attrs ? ["dot" "graphviz"]
        , extensions ? ["dot" "gv"]
      }:
        symlinkJoin {
          name = "dot";
          paths = [
            (callPackage ./kernel.nix { inherit graphviz attrs extensions; })
            (callPackage ./mode_info.nix { inherit attrs extensions; })
            graphviz
          ];
          passthru = {
            args = args // { baseName = x; };
            inherit meta packageOptions languageServerOptions;
          };
        };

      inherit meta;
    };
  }
) (lib.filter (x: lib.hasAttr x pkgs) baseCandidates))
