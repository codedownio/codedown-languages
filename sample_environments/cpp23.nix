{ codedown
, channels ? {}
, overlays ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "cpp23";
  inherit channels overlays;
  # metaOnly = true;

  kernels = [
    ({
      name = "cpp23";
      channel = "codedown";
      args = {
        packages = [];
        attrs = ["cpp23" "cpp"];
      };
    })
  ];

  otherPackages = [];
}
