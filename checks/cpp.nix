{
  kernels = [({
    channel = "nixpkgs";
    language = "cpp14";
    args = {
      packages = [];
      languageServers = [];
    };
  })];

  codeExecutions = map (x: x // { kernel = "cpp14"; }) [{
    code = ''
      #include <iostream>
      using namespace std;
      cout << "hi" << endl;
    '';
    output = "hi";
  }];
}
