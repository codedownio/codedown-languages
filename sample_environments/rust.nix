{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
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
