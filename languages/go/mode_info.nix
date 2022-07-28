{ lib
, writeTextDir
, attrs
, extensions
}:

writeTextDir "lib/codedown/go-modes.yaml" (lib.generators.toYAML {} [{
  inherit attrs extensions;
  code_mirror_mode = "go";
}])
