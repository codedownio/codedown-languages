{
  kernels = [({
    channel = "nixpkgs";
    name = "bash";
    args = {
      packages = [];
      languageServers = ["bashLanguageServer" "shellcheck"];
    };
  })];

  codeExecutions = map (x: x // { kernel = "bash"; }) [{
    code = ''echo hi'';
    output = "hi";
  }];
}
