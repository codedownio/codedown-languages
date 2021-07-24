{ lib
, writeTextDir
, attrs
, extensions
}:

writeTextDir "lib/codedown/octave-modes.yaml" (lib.generators.toYAML {} [{
  inherit attrs extensions;
  code_mirror_mode = "octave";
}])
