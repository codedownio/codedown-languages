{ lib
, writeTextDir
, attrs
, extensions
}:

writeTextDir "lib/codedown/haskell-modes.yaml" (lib.generators.toYAML {} [{
  inherit attrs extensions;
  code_mirror_mode = "haskell";
}])
