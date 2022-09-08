{ lib
, writeTextDir
, attrs
, extensions
}:

writeTextDir "lib/codedown/modes/python.yaml" (lib.generators.toYAML {} [{
  inherit attrs extensions;
  code_mirror_mode = "python";
}])
