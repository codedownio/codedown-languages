{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
  inherit channels;

  packages = {
    "codedown.kernels.rust" = {
      packages = {
        "rand" = {};
        "serde" = {
          features = ["derive"];
        };
      };
    };
  };
}
