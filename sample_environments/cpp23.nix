{ codedown
, channels ? {}
, overlays ? {}
, metaOnly ? false
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "cpp23";
  inherit channels overlays metaOnly;

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
