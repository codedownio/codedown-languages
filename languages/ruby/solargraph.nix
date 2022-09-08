{ pkgs
, rubyPackages
, solargraph ? rubyPackages.solargraph
, kernelName
}:

with pkgs;
with pkgs.lib;

let
  common = callPackage ../common.nix {};

in

common.writeTextDirWithMeta solargraph.meta "lib/codedown/language-servers/ruby-solargraph.yaml" (lib.generators.toYAML {} [{
  name = "solargraph";
  display_name = "Solargraph";
  description = "A Ruby language server";
  icon = ./logo-64x64.png;
  extensions = ["rb"];
  notebook_suffix = ".rb";
  kernel_name = kernelName;
  attrs = ["ruby"];
  type = "stream";
  args = ["${solargraph}/bin/solargraph" "stdio"];
}])