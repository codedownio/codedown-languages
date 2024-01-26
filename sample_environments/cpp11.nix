{ codedown
, channels ? {}
, overlays ? {}
, metaOnly ? false
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "cpp11";
  inherit channels overlays metaOnly;

  kernels = [
    ({
      name = "cpp11";
      channel = "codedown";
      args = {
        packages = [];
        attrs = ["cpp11" "cpp"];
      };
    })
  ];

  otherPackages = [];
}
