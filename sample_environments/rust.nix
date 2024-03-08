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
          {
            name = "serde";
            settings = {
              features = ["derive"];
            };
          }
        ];
      };
    })
  ];

  otherPackages = [];
}
