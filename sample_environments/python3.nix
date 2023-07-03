{ codedown
, channels ? {}
, overlays ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  inherit channels overlays;
  # metaOnly = true;

  kernels = [
    ({
      name = "python3";
      channel = "codedown";
      args = {
        packages = ["matplotlib" "scipy" "rope"];
        settings = {
          permitUserSite = false;
          "lsp.jedi.enable" = true;
          "lsp.pyright.enable" = true;
          "lsp.pylint.enable" = true;
          "lsp.flake8.enable" = true;
          "lsp.pycodestyle.enable" = true;
          "lsp.python-lsp-server.enable" = true;
        };
      };
    })
  ];

  otherPackages = [];
}
