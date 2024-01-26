{ codedown
, channels ? {}
, overlays ? {}
, metaOnly ? false
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "ruby";
  inherit channels overlays metaOnly;

  kernels = [
    ({
      name = "ruby";
      channel = "codedown";
      args = {
        packages = [];
      };
    })
  ];

  otherPackages = [];
}
