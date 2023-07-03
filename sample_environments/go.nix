{ codedown
, channels ? {}
, overlays ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "go";
  inherit channels overlays;
  # metaOnly = true;

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
