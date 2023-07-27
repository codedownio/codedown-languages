{ codedown
, channels ? {}
, overlays ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "python38";
  inherit channels overlays;
  # metaOnly = true;

  kernels = [
    ({
      name = "python38";
      channel = "codedown";
      args = {
        packages = [];
        settings = {
          "lsp.jedi.enable" = false;
          "enableVariableInspector" = true;

          # "lsp.python-lsp-server.enable" = true;
          # "lsp.pylint.enable" = true;
          # "lsp.pyright.enable" = true;
          # "lsp.pycodestyle.enable" = true;
        };
      };
    })
  ];

  otherPackages = [];
}
