{
  kernels = [({
    channel = "nixpkgs";
    language = "bash";
    args = {
      packages = [];
      languageServers = [];
    };
  })];

  codeExecutions = map (x: x // { kernel = "bash"; }) [{
    code = ''echo hi'';
    output = "hi";
  }];
}
