{ lib
, writeTextDir
, attrs
, extensions
}:

writeTextDir "lib/codedown/modes/octave.yaml" (lib.generators.toYAML {} [{
  inherit attrs extensions;
  code_mirror_mode = "octave";
}])
