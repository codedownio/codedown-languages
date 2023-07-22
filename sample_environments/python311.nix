{ codedown
, channels ? {}
, overlays ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "python311";
  inherit channels overlays;
  # metaOnly = true;

  kernels = [
    ({
      name = "python311";
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
