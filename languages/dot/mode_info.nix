{ lib
, writeTextDir
, attrs
, extensions
}:

writeTextDir "lib/codedown/dot-modes.yaml" (lib.generators.toYAML {} [{
  inherit attrs extensions;
  code_mirror_mode = "";
  codeMirrorMimeType = "text/plain";
}])
