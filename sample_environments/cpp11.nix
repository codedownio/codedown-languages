{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
  kernels.cpp.enable = true;
  kernels.cpp.flavor = "c++11";
}
