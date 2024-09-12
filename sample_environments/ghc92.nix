{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironmentPrime {
  inherit channels;

  packages = {
    "codedown.kernels.haskell-ghc92" = {
      packages = ["aeson"];
      settings = {
        "lsp.haskell-language-server.debug" = true;
      };
    };
  };
}
