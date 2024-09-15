{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment channels {
  kernels.rust.enable = true;
  kernels.rust.packages = [
    "rand"
    {
      name = "serde";
      settings = {
        features = ["derive"];
      };
    }
    "serde_json"
  ];
}
