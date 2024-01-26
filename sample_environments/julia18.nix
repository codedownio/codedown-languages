{ codedown
, channels ? {}
, overlays ? {}
, metaOnly ? false
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "julia18";
  inherit channels overlays metaOnly;

  kernels = [
    ({
      name = "julia18";
      channel = "codedown";
      args = {
        packages = ["JSON3" "Plots"];
      };
    })
  ];

  otherPackages = [];
}
