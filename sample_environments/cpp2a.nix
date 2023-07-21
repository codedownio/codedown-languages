{ codedown
, channels ? {}
, overlays ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "cpp2a";
  inherit channels overlays;
  # metaOnly = true;

  kernels = [
    ({
      name = "cpp2a";
      channel = "codedown";
      args = {
        packages = [];
        attrs = ["cpp2a" "cpp"];
      };
    })
  ];

  otherPackages = [];
}
