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
    attrName = "dot";
    codeMirrorMode = "";
    codeMirrorMimeType = "text/plain";
    extensionsToHighlight = ["dot" "gv"];
    extensionsToRun = ["dot" "gv"];
  }]);

  baseCandidates = [
    "graphviz"
  ];

in

lib.listToAttrs (map (x:
  let
    graphviz = lib.getAttr x pkgs;
  in {
    name = x;
    value = {
      packageOptions = {};
      packageSearch = common.searcher {};

      languageServerOptions = {};

      build = args@{
        packages ? []
        , languageServers ? []
        , codeDownAttr ? "dot"
        , otherLanguageKeys ? []
      }:
        symlinkJoin {
          name = "dot";
          paths = [
            (callPackage ./kernel.nix { inherit graphviz; })
            graphviz
            modeInfo
          ];
          passthru = {
            args = args // { baseName = x; };
            meta = graphviz.meta;
          };
        };

      meta = graphviz.meta // {
        baseName = "cpp11";
        displayName = "Graphviz " + graphviz.version;
        icon = ./logo-64x64.png;
      };
    };
  }
) (lib.filter (x: lib.hasAttr x pkgs) baseCandidates))
