{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironmentPrime {
  inherit channels;

  packages = {
    "codedown.kernels.julia16" = {
      packages = ["JSON3" "Plots"];
    };
  };
}
