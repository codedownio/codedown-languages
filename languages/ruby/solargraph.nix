{ rubyPackages
, solargraph ? rubyPackages.solargraph
}:

with pkgs;
with pkgs.lib;

let
  common = callPackage ../../common.nix {};

in

common.writeTextDirWithMeta solargraph.meta "lib/codedown/language-servers/ruby-solargraph.yaml" (lib.generators.toYAML {} [{
  name = "solargraph";
  display_name = "Solargraph";
  description = solargraph.meta.description;
  icon = ./logo.png;
  extensions = ["rb"];
  notebook_suffix = ".rb";
  kernel_name = kernelName;
  attrs = ["ruby"];
  type = "stream";
  args = ["${solargraph}/bin/solargraph" "stdio"];
}])
