{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironmentPrime {
  inherit channels;

  packages = {
    "codedown.kernels.cpp23" = {
      packages = [];
      attrs = ["cpp23" "cpp"];
    };
  };
}
