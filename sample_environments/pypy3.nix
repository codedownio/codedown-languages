{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "pypy3";

  kernels.python3.enable = true;
  kernels.python3.python3Package = "pypy3";
}
