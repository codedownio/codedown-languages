{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "cpp2c";

  kernels.cpp.enable = true;
  kernels.cpp.flavor = "c++2c";
}
