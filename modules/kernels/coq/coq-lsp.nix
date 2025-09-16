{ lib
, callPackage

, kernelName
}:

let
  common = callPackage ../common.nix {};

  coq-lsp = callPackage ./coq-lsp {};

  languageServerName = "coq-lsp";

  passthru = {
    inherit languageServerName;
  };

in

common.writeTextDirWithMetaAndPassthru coq-lsp.meta passthru "lib/codedown/language-servers/coq-${kernelName}-coq-lsp.yaml" (lib.generators.toYAML {} [{
  name = languageServerName;
  version = coq-lsp.version;
  extensions = ["sh" "bash"];
  notebook_suffix = ".bash";
  attrs = ["bash"];
  type = "stream";
  primary = true;
  args = [
    "${coq-lsp}/bin/coq-lsp"
  ];
}])
