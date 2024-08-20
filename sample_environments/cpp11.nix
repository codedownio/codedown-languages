{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
  inherit channels;

  packages = {
    "codedown.kernels.cpp11" = {
      packages = [];
      attrs = ["cpp11" "cpp"];
    };
  };
}
