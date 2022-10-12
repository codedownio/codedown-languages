{
  kernels = [({
    channel = "nixpkgs";
    name = "julia_15";
    args = {
      packages = [];
      languageServers = [];
    };
  })];

  codeExecutions = map (x: x // { kernel = "julia"; }) [{
    code = ''print("hi")'';
    output = "hi";
  }];
}
