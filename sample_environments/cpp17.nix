{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "cpp17";

  kernels.cpp.enable = true;
  kernels.cpp.flavor = "c++17";
}
