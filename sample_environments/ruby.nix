{ codedown
, channels ? {}
, overlays ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "ruby";
  inherit channels overlays;
  # metaOnly = true;

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
