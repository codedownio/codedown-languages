{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
  kernels.haskell.enable = true;
  kernels.haskell.ghcPackage = "ghc96";
  kernels.haskell.settings = {
    lsp.haskell-language-server.debug = true;
  };
}
