{
  kernels = [({
    channel = "nixpkgs-unstable";
    name = "python38";
    args = {
      packages = ["matplotlib" "scipy" "rope"];
      languageServers = ["jedi" "pyright" "pylint" "flake8" "pycodestyle" "microsoft" "python-lsp-server" "python-language-server"];
      settings = {
        permitUserSite = false;
      };
    };
  })];

  codeExecutions = map (x: x // { kernel = "python38"; }) [{
    code = "print(42)";
    output = "42";
  } {
    code = "import scipy";
    output = "";
  }];
}
