{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment channels {
  kernels.R.enable = true;
  kernels.R.packages = [
    "ggplot2"
  ];
}
