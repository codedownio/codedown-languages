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

in

common.writeTextDirWithMeta gopls.meta "lib/codedown/language-servers/gopls.yaml" (lib.generators.toYAML {} [{
  name = "gopls";
  display_name = "gopls";
  description = gopls.meta.description;
  icon = ./logo-64x64.png;
  extensions = ["go"];
  notebook_suffix = ".go";
  kernel_name = kernelName;
  header_lines = ["package Notebook"];
  attrs = attrs;
  type = "stream";
  args = ["${goplsWrapped}/bin/gopls"];
  env = {};
}])


# Deprecated: go-langserver
# https://github.com/sourcegraph/go-langserver
# common.writeTextDirWithMeta go-langserver.meta "lib/codedown/language-servers/go-langserver.yaml" (lib.generators.toYAML {} [{
#   name = "go-langserver";
#   display_name = "go-langserver";
#   description = go-langserver.meta.description;
#   icon = ./logo-64x64.png;
#   extensions = ["go"];
#   notebook_suffix = ".go";
#   kernel_name = kernelName;
#   header_lines = ["package Notebook"];
#   attrs = ["go"];
#   type = "stream";
#   args = ["${go-langserver}/bin/go-langserver"];
#   env = {};
# }])
