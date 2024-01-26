{ codedown
, channels ? {}
, overlays ? {}
, metaOnly ? false
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "cpp17";
  inherit channels overlays metaOnly;

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
