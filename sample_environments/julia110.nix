{ codedown
, ...
}:

codedown.makeEnvironment {
  kernels.julia.enable = true;
  kernels.julia.juliaPackage = "julia_110";
  kernels.julia.packages = ["JSON3" "Plots"];
}
