{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "go";

  kernels.go.enable = true;
  kernels.go.packages = [
    "rand"
  ];
}
