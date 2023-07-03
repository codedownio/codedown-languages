{ codedown
, channels ? {}
, overlays ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "bash";
  inherit channels overlays;
  # metaOnly = true;

  kernels = [
    ({
      name = "bashInteractive";
      channel = "codedown";
      args = {
        packages = [];
      };
    })
  ];

  otherPackages = [];
}
