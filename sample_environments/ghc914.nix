{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "ghc914";

  kernels.haskell.enable = true;
  kernels.haskell.ghcPackage = "ghc914";
  kernels.haskell.lsp.haskell-language-server.debug = true;
}
