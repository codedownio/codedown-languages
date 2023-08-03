{ codedown
, channels ? {}
, overlays ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "cpp20";
  inherit channels overlays;
  # metaOnly = true;

  kernels = [
    ({
      name = "cpp20";
      channel = "codedown";
      args = {
        packages = [];
        attrs = ["cpp20" "cpp"];
      };
    })
  ];

  otherPackages = [];
}
