{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
  inherit channels;

  packages = {
    "codedown.kernels.cpp20" = {
      packages = [];
      attrs = ["cpp20" "cpp"];
    };
  };
}
