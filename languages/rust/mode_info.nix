{ lib
, writeTextDir
, attrs
, extensions
}:

writeTextDir "lib/codedown/modes/rust.yaml" (lib.generators.toYAML {} [{
  inherit attrs extensions;
  code_mirror_mode = "rust";
}])
