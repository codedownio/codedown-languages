{ lib
, writeTextDir
, attrs
, extensions
}:

writeTextDir "lib/codedown/modes/r.yaml" (lib.generators.toYAML {} [{
  inherit attrs extensions;
  code_mirror_mode = "r";
}])
