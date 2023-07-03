{ codedown
, channels ? {}
, overlays ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  inherit channels overlays;
  # metaOnly = true;

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
