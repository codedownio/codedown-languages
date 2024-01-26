{ codedown
, channels ? {}
, overlays ? {}
, metaOnly ? false
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "ghc92";
  inherit channels overlays metaOnly;

  kernels = [
    ({
      name = "haskell-ghc92";
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
