{
  kernels = [({
    channel = "nixpkgs-unstable";
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
}
