{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "ghc96";

  kernels.haskell.enable = true;
  kernels.haskell.ghcPackage = "ghc96";
  kernels.haskell.lsp.haskell-language-server.debug = true;
}
