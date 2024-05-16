{ codedown
, channels ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "ghc98";
  inherit channels;

  kernels = [
    ({
      name = "haskell-ghc98";
      channel = "codedown";
      args = {
        packages = ["aeson"];
        settings = {
          "lsp.haskell-language-server.debug" = true;
        };
      };
    })
  ];

  otherPackages = [];
}
