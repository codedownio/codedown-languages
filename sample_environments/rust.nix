{ codedown
, channels ? {}
, overlays ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "rust";
  inherit channels overlays;

  kernels = [
    ({
      name = "rust";
      channel = "codedown";
      args = {
        packages = [
          "rand"
          { name = "serde"; features = ["derive"]; }
        ];
      };
    })
  ];

  otherPackages = [];
}
