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

  kernels.python3.permitUserSite = false;

  kernels.python3.lsp = {
    jedi.enable = true;
    pyright.enable = true;
    pylint.enable = true;
    flake8.enable = true;
    pycodestyle.enable = true;
    python-lsp-server.enable = true;
  };
}
