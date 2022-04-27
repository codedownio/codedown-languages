{
  kernels = [({
    channel = "nixpkgs";
    language = "cpp11";
    args = {
      packages = [];
      languageServers = [];
    };
  })];

  codeExecutions = map (x: x // { kernel = "cpp"; }) [{
    code = ''
      #include <iostream>
      using namespace std;
      cout << "hi" << endl;
    '';
    output = "hi";
  }];
}
