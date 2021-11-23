{ lib
, writeTextDir
, attrs
, extensions }:

writeTextDir "lib/codedown/clojure-modes.yaml" (lib.generators.toYAML {} [{
  inherit attrs extensions;
  code_mirror_mode = "clojure";
}])