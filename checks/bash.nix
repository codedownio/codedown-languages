{
  kernels = [({
    channel = "nixpkgs";
    language = "bashInteractive";
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
