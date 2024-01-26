{ codedown
, channels ? {}
, overlays ? {}
, metaOnly ? false
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "cpp14";
  inherit channels overlays metaOnly;

  kernels = [
    ({
      name = "cpp14";
      channel = "codedown";
      args = {
        packages = [];
        attrs = ["cpp14" "cpp"];
      };
    })
  ];

  otherPackages = [];
}
