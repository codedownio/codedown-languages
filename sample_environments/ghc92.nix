{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "ghc92";

  kernels.haskell.enable = true;
  kernels.haskell.ghcPackage = "ghc92";
  kernels.haskell.lsp.haskell-language-server.debug = true;
}
