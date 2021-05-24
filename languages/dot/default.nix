{pkgs, lib, callPackage, writeText, graphviz, symlinkJoin}:

rec {
  metadata = callPackage ./metadata.nix {};

  modeInfo = writeText "dot-mode-config.yaml" (lib.generators.toYAML {} [{
    attrName = "dot";
    codeMirrorMode = "";
    codeMirrorMimeType = "text/plain";
    extensionsToHighlight = ["dot" "gv"];
    extensionsToRun = ["dot" "gv"];
  }]);

  build = {
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
    };
}
