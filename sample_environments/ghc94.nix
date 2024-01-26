{ codedown
, channels ? {}
, overlays ? {}
, metaOnly ? false
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "ghc94";
  inherit channels overlays metaOnly;

  kernels = [
    ({
      name = "haskell-ghc94";
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
