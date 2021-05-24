{ lib, writeTextDir }:

writeTextDir "lib/r-mode-config.yaml" (lib.generators.toYAML {} [{
  attrName = "r";
  codeMirrorMode = "r";
  extensionsToHighlight = ["r"];
  extensionsToRun = ["r"];
} {
  attrName = "R";
  codeMirrorMode = "r";
  extensionsToHighlight = [];
  extensionsToRun = [];
}])
