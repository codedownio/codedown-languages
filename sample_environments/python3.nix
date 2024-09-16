{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
  kernels.python3.enable = true;
  kernels.python3.packages = [
    "matplotlib"
    "scipy"
    "rope"
  ];
  kernels.python3.settings = {
    permitUserSite = false;
    lsp.jedi.enable = true;
    lsp.pyright.enable = true;
    lsp.pylint.enable = true;
    lsp.flake8.enable = true;
    lsp.pycodestyle.enable = true;
    lsp.python-lsp-server.enable = true;
  };
}
