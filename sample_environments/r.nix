{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "r";

  kernels.R.enable = true;
  kernels.R.packages = [
    "ggplot2"
  ];
}
