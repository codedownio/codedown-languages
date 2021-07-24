{ lib
, writeTextDir
, attrs
, extensions
}:

writeTextDir "lib/codedown/rust-modes.yaml" (lib.generators.toYAML {} [{
  inherit attrs extensions;
  code_mirror_mode = "rust";
}])
