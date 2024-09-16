{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
  kernels.haskell.enable = true;
  kernels.haskell.ghcPackage = "ghc98";
  kernels.haskell.settings = {
    lsp.haskell-language-server.debug = true;
  };
}
