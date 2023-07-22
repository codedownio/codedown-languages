{ codedown
, channels ? {}
, overlays ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "python27";
  inherit channels overlays;
  # metaOnly = true;

  kernels = [
    ({
      name = "python27";
      channel = "codedown";
      args = {
        packages = [];
        settings = {
          "lsp.jedi.enable" = false;
          "enableVariableInspector" = false;
        };
      };
    })
  ];

  otherPackages = [];
}
