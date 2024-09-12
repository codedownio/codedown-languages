{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironmentPrime {
  inherit channels;

  packages = {
    "codedown.kernels.cpp14" = {
      packages = [];
      attrs = ["cpp14" "cpp"];
    };
  };
}
