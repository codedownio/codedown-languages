{ lib
, writeTextDir
, attrs
, extensions
}:


writeTextDir "lib/codedown/postgres-modes.yaml" (lib.generators.toYAML {} [{
  inherit attrs extensions;
  code_mirror_mode = "sql";
}])
