{ codedown
, ...
}:

codedown.makeEnvironment {
  kernels.go.enable = true;
  kernels.go.packages = [
    "rand"
  ];
}
