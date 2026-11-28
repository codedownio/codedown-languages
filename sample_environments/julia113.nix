{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "julia113";

  kernels.julia.enable = true;
  kernels.julia.juliaPackage = "julia_113";
  # Plots needs a newer General registry to build on Julia 1.13; see julia-modules/VENDORED.md.
  kernels.julia.packages = ["JSON3"];
}
