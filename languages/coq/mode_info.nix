{ lib
, writeTextDir
, attrs
, extensions
}:

writeTextDir "lib/codedown/modes/coq.yaml" (lib.generators.toYAML {} [{
  inherit attrs extensions;
  code_mirror_mode = "coq";
}])
