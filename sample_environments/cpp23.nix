{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "cpp23";

  kernels.cpp.enable = true;
  kernels.cpp.flavor = "c++23";
}
