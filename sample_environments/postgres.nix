{ codedown
, channels ? {}
, overlays ? {}
, metaOnly ? false
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "postgres";
  inherit channels overlays metaOnly;

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
