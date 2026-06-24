{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "r-ark";

  kernels.R-ark.enable = true;
  kernels.R-ark.packages = [
    "jsonlite"
  ];
}
