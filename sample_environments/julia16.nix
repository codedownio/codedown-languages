{ codedown
, channels ? {}
, overlays ? {}
, metaOnly ? false
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "julia16";
  inherit channels overlays metaOnly;

  kernels = [
    ({
      name = "julia16";
      channel = "codedown";
      args = {
        packages = ["JSON3" "Plots"];
      };
    })
  ];

  otherPackages = [];
}
