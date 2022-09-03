{ lib
, writeTextDir
, attrs
, extensions
}:


writeTextDir "lib/codedown/modes/bash.yaml" (lib.generators.toYAML {} [{
  inherit attrs extensions;
  code_mirror_mode = "shell";
}])
