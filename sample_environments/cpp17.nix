{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
  inherit channels;

  packages = {
    "codedown.kernels.cpp17" = {
      packages = [];
      attrs = ["cpp17" "cpp"];
    };
  };
}
