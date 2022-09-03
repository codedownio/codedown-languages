{ lib
, writeTextDir
, attrs
, extensions
}:

writeTextDir "lib/codedown/modes/haskell.yaml" (lib.generators.toYAML {} [{
  inherit attrs extensions;
  code_mirror_mode = "haskell";
}])
