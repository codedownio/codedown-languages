{ codedown
, channels ? {}
, overlays ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "cpp17";
  inherit channels overlays;
  # metaOnly = true;

  kernels = [
    ({
      name = "cpp17";
      channel = "codedown";
      args = {
        packages = [];
        attrs = ["cpp17" "cpp"];
      };
    })
  ];

  otherPackages = [];
}
