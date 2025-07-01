{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "cpp11";

  kernels.cpp.enable = true;
  kernels.cpp.flavor = "c++11";
}
