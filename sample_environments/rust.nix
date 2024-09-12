{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironmentPrime {
  inherit channels;

  packages = {
    "codedown.kernels.rust" = {
      packages = {
        "rand" = {};
        "serde" = {
          # features = ["std" "derive"];
          features = ["derive"];
        };
        "serde_json" = {};
      };
    };
  };
}
