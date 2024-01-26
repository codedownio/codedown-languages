{ codedown
, channels ? {}
, overlays ? {}
, metaOnly ? false
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "cpp20";
  inherit channels overlays metaOnly;

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
