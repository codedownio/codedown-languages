{ lib, writeTextDir }:

writeTextDir "lib/codedown/r-modes.yaml" (lib.generators.toYAML {} [{
  attr_name = "r";
  code_mirror_mode = "r";
  extensions_to_highlight = ["r"];
  extensions_to_run = ["r"];
} {
  attr_name = "R";
  code_mirror_mode = "r";
  extensions_to_highlight = [];
  extensions_to_run = [];
}])
