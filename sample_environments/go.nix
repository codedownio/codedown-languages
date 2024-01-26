{ codedown
, channels ? {}
, overlays ? {}
, metaOnly ? false
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "go";
  inherit channels overlays metaOnly;

  kernels = [
    ({
      name = "go";
      channel = "codedown";
      args = {
        packages = ["rand"];
      };
    })
  ];

  otherPackages = [];
}
