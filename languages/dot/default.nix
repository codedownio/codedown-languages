{ pkgs
, lib
, callPackage
, writeTextDir
, graphviz
, symlinkJoin
}:

let
  common = callPackage ../common.nix {};

  modeInfo = writeTextDir "lib/codedown/dot-modes.yaml" (lib.generators.toYAML {} [{
    attr_name = "dot";
    code_mirror_mode = "";
    codeMirrorMimeType = "text/plain";
    extensions_to_highlight = ["dot" "gv"];
    extensions_to_run = ["dot" "gv"];
  }]);

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
      }:
        symlinkJoin {
          name = "dot";
          paths = [
            (callPackage ./kernel.nix { inherit graphviz attrs; })
            graphviz
            modeInfo
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
