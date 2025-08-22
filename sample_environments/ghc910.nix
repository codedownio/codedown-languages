{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "ghc910";

  kernels.haskell.enable = true;
  kernels.haskell.ghcPackage = "ghc910";
  kernels.haskell.lsp.haskell-language-server.debug = true;
}
