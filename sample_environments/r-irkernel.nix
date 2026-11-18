{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "r-irkernel";

  kernels.R.enable = true;
  kernels.R.backend = "irkernel";
  kernels.R.packages = [
    "ggplot2"
  ];
}
