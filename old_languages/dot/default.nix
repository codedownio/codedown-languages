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
      baseName = "graphviz";
      displayName = "Graphviz " + graphviz.version;
      version = graphviz.version;
      icon = ./dot-logo-64x64.png;
    };

  in {
    name = x;
    value = rec {
      packageOptions = {};
      packageSearch = common.searcher {};
      versions = {
        graphviz = graphviz.version;
      };

      build = args@{
        packages ? []
        , settings ? {}
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
            inherit meta packageOptions;
            modes = {
              inherit attrs extensions;
              code_mirror_mode = "";
              codeMirrorMimeType = "text/plain";
            };
          };
        };

      inherit meta;
    };
  }
) (lib.filter (x: lib.hasAttr x pkgs) baseCandidates))
