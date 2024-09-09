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
          # features = ["std" "derive"];
          features = ["derive"];
        };
        "serde_json" = {};
      };
    };
  };
}
