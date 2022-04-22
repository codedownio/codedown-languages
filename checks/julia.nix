{
  kernels = [({
    channel = "nixpkgs";
    language = "julia_15";
    args = {
      packages = [];
      languageServers = [];
    };
  })];

  codeExecutions = map (x: x // { kernel = "julia"; }) [{
    code = ''putStrLn "hi"'';
    output = "hi";
  }];
}
