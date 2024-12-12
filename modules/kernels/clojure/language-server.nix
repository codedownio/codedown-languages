{ lib
, callPackage

, clojure-lsp

, kernelName
}:

let
  common = callPackage ../common.nix {};

  languageServerName = "clojure-lsp";

  passthru = {
    inherit languageServerName;
  };

in

common.writeTextDirWithMetaAndPassthru clojure-lsp.meta passthru "lib/codedown/language-servers/clojure-${kernelName}-clojure-lsp.yaml" (lib.generators.toYAML {} [{
  name = languageServerName;
  version = clojure-lsp.version;
  display_name = "Clojure LSP";
  description = clojure-lsp.meta.description or "Clojure & ClojureScript Language Server (LSP) implementation";
  icon = ./clojure-logo-64x64.png;
  extensions = ["clj"];
  notebook_suffix = ".clj";
  kernel_name = kernelName;
  attrs = ["clojure"];
  type = "stream";
  args = ["${clojure-lsp}/bin/clojure-lsp"];
  env = {};
}])
