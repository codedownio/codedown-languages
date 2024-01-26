{ codedown
, channels ? {}
, overlays ? {}
, metaOnly ? false
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "ghc96";
  inherit channels overlays metaOnly;

  kernels = [
    ({
      name = "haskell-ghc96";
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
