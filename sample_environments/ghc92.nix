{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
  kernels.haskell.enable = true;
  kernels.haskell.ghcPackage = "ghc92";
  kernels.haskell.settings = {
    lsp.haskell-language-server.debug = true;
  };
}
