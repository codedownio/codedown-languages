{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "pypy3";

  # kernels.pypy3.enable = true;
  # kernels.pypy3.packages = [
  #   "matplotlib"
  #   "scipy"
  #   "rope"
  # ];
}
