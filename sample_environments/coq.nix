{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment channels {
  kernels.coq.enable = true;
  # kernels.coq.coqPackages = "coqPackages_8_17";
}
