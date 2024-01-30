{ codedown
, channels ? {}
, overlays ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "cpp14";
  inherit channels overlays;

  kernels = [
    ({
      name = "cpp14";
      channel = "codedown";
      args = {
        packages = [];
        attrs = ["cpp14" "cpp"];
      };
    })
  ];

  otherPackages = [];
}
