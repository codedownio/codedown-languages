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

    codeExecutions = map (x: x // { kernel = "python38"; }) [{
      code = "print(42)";
      output = "42";
    } {
      code = "import scipy";
      output = "";
    }];
  };

  haskell = {
    kernels = [({
      channel = "nixpkgs";
      language = "haskell-ghc902";
      args = {
        packages = ["aeson"];
        languageServers = ["haskell-language-server"];
      };
    })];

    codeExecutions = map (x: x // { kernel = "haskell-ghc902"; }) [{
      code = ''putStrLn "hi"'';
      output = "hi";
    } {
      code = "import Data.Aeson";
      output = "";
    }];
  };
}
