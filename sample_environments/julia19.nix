{ codedown
, channels ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "julia19";
  inherit channels;

  kernels = [
    ({
      name = "julia19";
      channel = "codedown";
      args = {
        packages = ["JSON3" "Plots" "RDatasets"];
        # settings = {
        #   precompile = false;
        # };
      };
    })
  ];

  otherPackages = [];
}
