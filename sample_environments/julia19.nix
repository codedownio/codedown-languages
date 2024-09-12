{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironmentPrime {
  inherit channels;

  packages = {
    "codedown.kernels.julia19" = {
      packages = ["JSON3" "Plots" "RDatasets"];
    };
  };
}
