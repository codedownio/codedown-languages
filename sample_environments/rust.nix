{ codedown
, channels ? {}
, overlays ? {}
, metaOnly ? false
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "rust";
  inherit channels overlays metaOnly;

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
