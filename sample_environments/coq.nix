{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironmentPrime {
  inherit channels;

  packages = {
    "codedown.kernels.coq" = {
      packages = {};
    };
  };
}
