{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "typescript";

  kernels.typescript.enable = true;
  kernels.typescript.packages = [
    "d3"
    "jsdom"
    "@types/d3"
    "simple-statistics"
  ];
}
