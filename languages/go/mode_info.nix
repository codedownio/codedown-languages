{ lib
, writeTextDir
, attrs
, extensions
}:

writeTextDir "lib/codedown/modes/go.yaml" (lib.generators.toYAML {} [{
  inherit attrs extensions;
  code_mirror_mode = "go";
}])
