{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "ghc912";

  kernels.haskell.enable = true;
  kernels.haskell.ghcPackage = "ghc912";
  kernels.haskell.lsp.haskell-language-server.debug = true;
}
