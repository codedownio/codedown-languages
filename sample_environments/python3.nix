{ codedown
, channels ? {}
, pkgsStable
, ...
}:

codedown.makeEnvironmentPrime {
  inherit channels;

  packages = {
    "codedown.kernels.python3" = {
      packages = {
        "matplotlib" = {};
        "scipy" = {};
        "rope" = {};
      };

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

    "nixpkgs.htop" = {};
  };
}
