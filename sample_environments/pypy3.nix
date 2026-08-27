{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "pypy3";

  kernels.pypy3.enable = true;
}
