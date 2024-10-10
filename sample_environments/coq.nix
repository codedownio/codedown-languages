{ codedown
, ...
}:

codedown.makeEnvironment {
  kernels.coq.enable = true;
  # kernels.coq.coqPackages = "coqPackages_8_17";
}
