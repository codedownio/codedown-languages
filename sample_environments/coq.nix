{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "coq";

  kernels.coq.enable = true;
  kernels.coq.packages = ["bignums"];
  # kernels.coq.coqPackages = "coqPackages_8_17";
}
