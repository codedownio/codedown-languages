{ lib
, callPackage
, runCommand
, makeWrapper
, pkgs

, attrs
, kernelName

, go
, gopls
# , go-langserver
}:

let
  common = callPackage ../common.nix {};

  goplsWrapped = runCommand "gopls-wrapped" { buildInputs = [makeWrapper]; } ''
    mkdir -p $out/bin
    makeWrapper ${gopls}/bin/gopls $out/bin/gopls \
      --suffix PATH ":" ${go}/bin \
      --set GOCACHE /tmp/gocache \
      --set GOPATH ${go}/share/go
  '';

  languageServerName = "gopls";

  passthru = {
    inherit languageServerName;
  };

in

common.writeTextDirWithMetaAndPassthru gopls.meta passthru "lib/codedown/language-servers/go-${kernelName}-gopls.yaml" (lib.generators.toYAML {} [{
  name = languageServerName;
  version = gopls.version;
  display_name = "gopls";
  description = gopls.meta.description;
  icon = ./go-logo-64x64.png;
  extensions = ["go"];
  notebook_suffix = ".go";
  kernel_name = kernelName;
  header_lines = ["package Notebook"];
  attrs = attrs;
  type = "stream";
  args = ["${goplsWrapped}/bin/gopls"];
  env = {};
  language_id = "go";
}])


# Deprecated: go-langserver
# https://github.com/sourcegraph/go-langserver
# common.writeTextDirWithMeta go-langserver.meta "lib/codedown/language-servers/go-langserver.yaml" (lib.generators.toYAML {} [{
#   name = "go-langserver";
#   display_name = "go-langserver";
#   description = go-langserver.meta.description;
#   icon = ./go-logo-64x64.png;
#   extensions = ["go"];
#   notebook_suffix = ".go";
#   kernel_name = kernelName;
#   header_lines = ["package Notebook"];
#   attrs = ["go"];
#   type = "stream";
#   args = ["${go-langserver}/bin/go-langserver"];
#   env = {};
# }])
