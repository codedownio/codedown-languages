{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
  inherit channels;

  packages = {
    "codedown.kernels.R" = {
      packages = {
        "ggplot2" = {};
      };
    };
  };
}
