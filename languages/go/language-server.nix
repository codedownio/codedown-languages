{ callPackage
, lib
, pkgs
, kernelName
, go-langserver
}:

let
  common = callPackage ../common.nix {};

in

common.writeTextDirWithMeta go-langserver.meta "lib/codedown/language-servers/go.yaml" (lib.generators.toYAML {} [{
  name = "go-langserver";
  display_name = "go-langserver";
  description = go-langserver.meta.description;
  icon = ./logo-64x64.png;
  extensions = ["go"];
  notebook_suffix = ".go";
  kernel_name = kernelName;
  attrs = ["go"];
  type = "stream";
  args = ["${go-langserver}/bin/go-langserver"];
  env = {};
}])
