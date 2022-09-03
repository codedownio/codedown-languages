{ lib
, writeTextDir
, attrs
, extensions
}:

writeTextDir "lib/codedown/modes/julia.yaml" (lib.generators.toYAML {} [{
  inherit attrs extensions;
  code_mirror_mode = "julia";
}])
