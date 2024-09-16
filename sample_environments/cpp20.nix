{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment channels {
  kernels.cpp.enable = true;
  kernels.cpp.flavor = "c++20";
}
