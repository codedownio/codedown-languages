{ lib
, callPackage
, kernelName
}:

let
  common = callPackage ../common.nix {};

  clojure-lsp = (builtins.getFlake "github:codedownio/clojure-lsp/e47c4cd8c7b30dfa3b13021bf6ec90c8c81bf041").packages.x86_64-linux.default;

in

common.writeTextDirWithMeta clojure-lsp.meta "lib/codedown/language-servers/clojure-lsp.yaml" (lib.generators.toYAML {} [{
  name = "clojure-lsp";
  display_name = "Clojure LSP";
  description = clojure-lsp.meta.description or "Clojure & ClojureScript Language Server (LSP) implementation";
  icon = ./logo-64x64.png;
  extensions = ["clj"];
  notebook_suffix = ".clj";
  kernel_name = kernelName;
  attrs = ["clojure"];
  type = "stream";
  args = ["${clojure-lsp}/bin/clojure-lsp"];
  env = {};
}])
