{ codedown
, channels ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "rust";
  inherit channels;

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
