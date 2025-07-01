{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "rust";

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
