{ lib
, writeTextDir
, attrs
, extensions
}:

writeTextDir "lib/codedown/r-modes.yaml" (lib.generators.toYAML {} [{
  inherit attrs extensions;
  code_mirror_mode = "r";
}])
