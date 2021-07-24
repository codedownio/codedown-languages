{ lib
, writeTextDir
, attrs
, extensions
}:


writeTextDir "lib/codedown/bash-modes.yaml" (lib.generators.toYAML {} [{
  inherit attrs extensions;
  code_mirror_mode = "shell";
}])
