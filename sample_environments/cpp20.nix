{ codedown
, ...
}:

codedown.makeEnvironment {
  kernels.cpp.enable = true;
  kernels.cpp.flavor = "c++20";
}
