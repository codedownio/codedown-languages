{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "ghc94";

  kernels.haskell.enable = true;
  kernels.haskell.ghcPackage = "ghc94";
  kernels.haskell.lsp.haskell-language-server.debug = true;
}
