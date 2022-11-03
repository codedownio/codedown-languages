{ lib
, callPackage
, kernelName
, clojure-lsp
}:

let
  common = callPackage ../common.nix {};

in

common.writeTextDirWithMeta clojure-lsp.meta "lib/codedown/language-servers/clojure-lsp.yaml" (lib.generators.toYAML {} [{
  name = "clojure-lsp";
  display_name = "Clojure LSP";
  description = clojure-lsp.meta.description;
  icon = ./logo-64x64.png;
  extensions = ["clj"];
  notebook_suffix = ".clj";
  kernel_name = kernelName;
  attrs = ["clojure"];
  type = "stream";
  args = ["${clojure-lsp}/bin/clojure-lsp"];
  env = {};
}])
