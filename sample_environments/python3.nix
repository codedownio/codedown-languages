{ codedown
, channels ? {}
, pkgsStable
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "python3";
  inherit channels;

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

  otherPackages = [
    { channel = "nixpkgs"; attr = "htop"; contents = pkgsStable.htop; }
  ];
}
