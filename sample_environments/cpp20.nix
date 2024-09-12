{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironmentPrime {
  inherit channels;

  packages = {
    "codedown.kernels.cpp20" = {
      packages = [];
      attrs = ["cpp20" "cpp"];
    };
  };
}
