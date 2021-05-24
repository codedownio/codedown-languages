{ lib, writeText }:

writeText "mode_config.yaml" (lib.generators.toYAML {} [{
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
