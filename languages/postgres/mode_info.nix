{ lib
, writeTextDir
, attrs
, extensions
}:


writeTextDir "lib/codedown/modes/postgres.yaml" (lib.generators.toYAML {} [{
  inherit attrs extensions;
  code_mirror_mode = "sql";
}])
