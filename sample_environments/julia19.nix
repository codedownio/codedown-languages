{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
  kernels.julia.enable = true;
  kernels.julia.juliaPackage = "julia_19";
  kernels.julia.packages = ["JSON3" "Plots"];
}
