{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
  inherit channels;

  packages = {
    "codedown.kernels.cpp14" = {
      packages = [];
      attrs = ["cpp14" "cpp"];
    };
  };
}
