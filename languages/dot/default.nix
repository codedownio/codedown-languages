{ pkgs
, lib
, callPackage
, writeTextDir
, graphviz
, symlinkJoin
}:

rec {
  metadata = callPackage ./metadata.nix {};

  modeInfo = writeTextDir "lib/codedown/dot-modes.yaml" (lib.generators.toYAML {} [{
    attrName = "dot";
    codeMirrorMode = "";
    codeMirrorMimeType = "text/plain";
    extensionsToHighlight = ["dot" "gv"];
    extensionsToRun = ["dot" "gv"];
  }]);

  build = args@{
    baseName
    , packages ? (_: [])
    , languageServers ? (_: [])
    , codeDownAttr ? "dot"
    , otherLanguageKeys ? []
  }:
    let
      base = pkgs.lib.findSingle (x: x.name == baseName) null "multiple" metadata.baseOptions;
      graphviz = base.graphviz;

    in symlinkJoin {
      name = "dot";
      paths = [
        (callPackage ./kernel.nix { inherit graphviz; })
        graphviz
        modeInfo
      ];
      passthru = {
        inherit args metadata;
        meta = base.meta;
      };
    };
}
