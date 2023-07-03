{ codedown
, channels ? {}
, overlays ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "rust";
  inherit channels overlays;
  # metaOnly = true;

  kernels = [
    ({
      name = "rust";
      channel = "codedown";
      args = {
        packages = ["rand"];
      };
    })
  ];

  otherPackages = [];
}
