{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
  kernels.pypy3.enable = true;
  kernels.pypy3.packages = [
    "matplotlib"
    "scipy"
    "rope"
  ];
}
