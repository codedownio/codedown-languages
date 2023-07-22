{ codedown
, channels ? {}
, overlays ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "python310";
  inherit channels overlays;
  # metaOnly = true;

  kernels = [
    ({
      name = "python310";
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
