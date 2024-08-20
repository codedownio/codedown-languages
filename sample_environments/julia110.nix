{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
  inherit channels;

  packages = {
    "codedown.kernels.julia110" = {
      packages = ["JSON3" "Plots" "RDatasets"];
    };
  };
}
