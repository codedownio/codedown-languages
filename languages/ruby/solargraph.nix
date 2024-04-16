{ callPackage
, lib

, rubyPackages
, solargraph ? rubyPackages.solargraph
, kernelName
}:

let
  common = callPackage ../common.nix {};

  languageServerName = "solargraph";

  passthru = {
    inherit languageServerName;
  };

in

common.writeTextDirWithMetaAndPassthru solargraph.meta passthru "lib/codedown/language-servers/ruby-solargraph.yaml" (lib.generators.toYAML {} [{
  name = languageServerName;
  version = solargraph.version;
  display_name = "Solargraph";
  description = "A Ruby language server";
  icon = ./iruby-64x64.png;
  extensions = ["rb"];
  notebook_suffix = ".rb";
  kernel_name = kernelName;
  attrs = ["ruby"];
  type = "stream";
  args = ["${solargraph}/bin/solargraph" "stdio"];
}])
