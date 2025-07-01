{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "coq";

  kernels.coq.enable = true;
  # kernels.coq.coqPackages = "coqPackages_8_17";
}
