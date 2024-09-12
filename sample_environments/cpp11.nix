{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironmentPrime {
  inherit channels;

  packages = {
    "codedown.kernels.cpp11" = {
      packages = [];
      attrs = ["cpp11" "cpp"];
    };
  };
}
