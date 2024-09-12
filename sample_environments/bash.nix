{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironmentPrime {
  inherit channels;

  packages = {
    "codedown.kernels.bash" = {
      packages = {};
    };
  };
}
