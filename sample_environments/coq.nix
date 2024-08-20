{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
  inherit channels;

  packages = {
    "codedown.kernels.coq" = {
      packages = {};
    };
  };
}
