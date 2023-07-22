{ codedown
, channels ? {}
, overlays ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "python312";
  inherit channels overlays;
  # metaOnly = true;

  kernels = [
    ({
      name = "python312";
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
