{ codedown
, channels ? {}
, overlays ? {}
, metaOnly ? false
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "coq";
  inherit channels overlays metaOnly;

  kernels = [
    ({
      name = "coq";
      channel = "codedown";
      args = {
        packages = [];
      };
    })
  ];

  otherPackages = [];
}
