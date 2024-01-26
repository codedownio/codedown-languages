{ codedown
, channels ? {}
, overlays ? {}
, metaOnly ? false
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "r";
  inherit channels overlays metaOnly;

  kernels = [
    ({
      name = "R";
      channel = "codedown";
      args = {
        packages = [];
      };
    })
  ];

  otherPackages = [];
}
