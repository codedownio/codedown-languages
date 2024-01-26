{ codedown
, channels ? {}
, overlays ? {}
, metaOnly ? false
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "julia19";
  inherit channels overlays metaOnly;

  kernels = [
    ({
      name = "julia19";
      channel = "codedown";
      args = {
        packages = ["JSON3" "Plots"];
      };
    })
  ];

  otherPackages = [];
}
