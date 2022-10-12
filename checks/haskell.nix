
let
  code = [{
    code = ''putStrLn "hi"'';
    output = "hi";
  } {
    code = "import Data.Aeson";
    output = "";
  }];

in

{
  kernels = [{
    channel = "nixpkgs-unstable";
    name = "haskell-ghc902";
    args = {
      packages = ["aeson"];
      languageServers = ["haskell-language-server"];
    };
  } {
    channel = "nixpkgs-unstable";
    name = "haskell-ghc8107";
    args = {
      packages = ["aeson"];
      languageServers = ["haskell-language-server"];
    };
  }];

  codeExecutions =
    (map (x: x // { kernel = "haskell-ghc902"; }) code)
    ++ (map (x: x // { kernel = "haskell-ghc8107"; }) code)
  ;
}
