{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "lean4";

  kernels.lean4.enable = true;
  kernels.lean4.packages = [
    "batteries"
  ];
}
