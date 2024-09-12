{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironmentPrime {
  inherit channels;

  packages = {
    "codedown.kernels.haskell-ghc98" = {
      packages = ["aeson"];
      settings = {
        "lsp.haskell-language-server.debug" = true;
      };
    };
  };
}
