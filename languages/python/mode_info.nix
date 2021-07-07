{lib, writeTextDir}:

writeTextDir "lib/codedown/python-modes.yaml" (lib.generators.toYAML {} [{
  attr_name = "python";
  code_mirror_mode = "python";
  extensions_to_highlight = ["py"];
  extensions_to_run = ["py"];
} {
  attr_name = "python2";
  code_mirror_mode = "python";
  extensions_to_highlight = ["py"];
  extensions_to_run = ["py"];
} {
  attr_name = "python3";
  code_mirror_mode = "python";
  extensions_to_highlight = ["py"];
  extensions_to_run = ["py"];
}])
