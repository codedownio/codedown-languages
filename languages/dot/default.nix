{pkgs, lib, callPackage, writeText, graphviz}:

rec {
  metadata = callPackage ./metadata.nix {};

  build = {
    baseName,
    packages ? (_: []),
    languageServers ? (_: []),
    codeDownAttr ? "dot",
    otherLanguageKeys ? []
  }:
    let
      base = pkgs.lib.findSingle (x: x.name == baseName) null "multiple" metadata.baseOptions;
      graphviz = base.graphviz;
    in {
      name = "dot";
      binaries = [graphviz];
      kernel = callPackage ./kernel.nix { inherit graphviz; };
      modeInfo = writeText "mode_config.yaml" (lib.generators.toYAML {} [{
        attrName = "dot";
        codeMirrorMode = "";
        codeMirrorMimeType = "text/plain";
        extensionsToHighlight = ["dot" "gv"];
        extensionsToRun = ["dot" "gv"];
      }]);
    };
}
