{ codedown
, ...
}:

codedown.makeEnvironment {
  kernels.rust.enable = true;
  kernels.rust.packages = [
    "rand"
    {
      name = "serde";
      features = ["derive"];
    }
    "serde_json"
  ];
}
