{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment channels {
  kernels.haskell.enable = true;
  kernels.haskell.ghcPackage = "ghc96";
  kernels.haskell.settings = {
    lsp.haskell-language-server.debug = true;
  };
}
