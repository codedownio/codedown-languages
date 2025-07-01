{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "cpp20";

  kernels.cpp.enable = true;
  kernels.cpp.flavor = "c++20";
}
