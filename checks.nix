{
  python = {
    kernels = [({
      channel = "nixpkgs-unstable";
      language = "python38";
      args = {
        packages = ["matplotlib" "scipy" "rope"];
        languageServers = ["jedi" "pyright" "pylint" "flake8" "pycodestyle" "microsoft" "python-lsp-server" "python-language-server"];
        settings = {
          permitUserSite = false;
        };
      };
    })];

    codeExecutions = {
      python38 = [{
        code = "print(42)";
        output = "42";
      }];
    };
  };
}
