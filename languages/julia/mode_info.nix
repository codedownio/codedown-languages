{ lib
, writeTextDir
, attrs
, extensions
}:

writeTextDir "lib/codedown/julia-modes.yaml" (lib.generators.toYAML {} [{
  inherit attrs extensions;
  code_mirror_mode = "julia";
}])
