{ lib
, writeTextDir
, attrs
, extensions
}:

writeTextDir "lib/codedown/modes/cpp.yaml" (lib.generators.toYAML {} [{
  inherit attrs extensions;
  code_mirror_mode = "clike";
  code_mirror_mime_type = "text/x-c++src";
}])
