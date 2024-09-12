{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironmentPrime {
  inherit channels;

  packages = {
    "codedown.kernels.cpp17" = {
      packages = [];
      attrs = ["cpp17" "cpp"];
    };
  };
}
