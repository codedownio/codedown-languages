{ codedown
, ...
}:

codedown.makeEnvironment {
  kernels.R.enable = true;
  kernels.R.packages = [
    "ggplot2"
  ];
}
