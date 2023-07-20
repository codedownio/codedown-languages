{ codedown
, channels ? {}
, overlays ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "python38";
  inherit channels overlays;
  # metaOnly = true;

  kernels = [
    ({
      name = "python38";
      channel = "codedown";
      args = {
        packages = [];
        settings = {};
      };
    })
  ];

  otherPackages = [];
}
