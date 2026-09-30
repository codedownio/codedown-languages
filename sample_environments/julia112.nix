{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "julia112";

  kernels.julia.enable = true;
  kernels.julia.juliaPackage = "julia_112";
  kernels.julia.packages = ["JSON3" "Plots"];
}
