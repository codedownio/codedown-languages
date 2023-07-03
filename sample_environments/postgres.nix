{ codedown
, channels ? {}
, overlays ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "postgres";
  inherit channels overlays;
  # metaOnly = true;

  kernels = [
    ({
      name = "postgres";
      channel = "codedown";
      args = {
        packages = [];
      };
    })
  ];

  otherPackages = [];
}
