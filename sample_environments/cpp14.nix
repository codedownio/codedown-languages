{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "cpp14";

  kernels.cpp.enable = true;
  kernels.cpp.flavor = "c++14";
}
