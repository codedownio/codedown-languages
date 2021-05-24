{lib, writeTextDir}:

writeTextDir "lib/codedown/python-mode-config.yaml" (lib.generators.toYAML {} [{
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
