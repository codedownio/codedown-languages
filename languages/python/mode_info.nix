{stdenv, pkgs}:

with pkgs;

writeText "mode_config.yaml" (pkgs.lib.generators.toYAML {} [{
  attrName = "python";
  codeMirrorMode = "python";
  extensionsToHighlight = [];
  extensionsToRun = [];
} {
  attrName = "python2";
  codeMirrorMode = "python";
  extensionsToHighlight = [];
  extensionsToRun = [];
} {
  attrName = "python3";
  codeMirrorMode = "python";
  extensionsToHighlight = ["py"];
  extensionsToRun = ["py"];
}])
