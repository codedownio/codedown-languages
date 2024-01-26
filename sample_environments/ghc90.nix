{ codedown
, channels ? {}
, overlays ? {}
, metaOnly ? false
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "ghc90";
  inherit channels overlays metaOnly;

  kernels = [
    ({
      name = "haskell-ghc90";
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
