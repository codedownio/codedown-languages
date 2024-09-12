{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironmentPrime {
  inherit channels;

  packages = {
    "codedown.kernels.R" = {
      packages = {
        "ggplot2" = {};
      };
    };
  };
}
