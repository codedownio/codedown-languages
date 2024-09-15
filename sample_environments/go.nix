{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment channels {
  kernels.go.enable = true;
  kernels.go.packages = [
    "rand"
  ];
}
