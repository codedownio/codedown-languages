{ codedown
, channels ? {}
, overlays ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  inherit channels overlays;
  # metaOnly = true;

  kernels = [
    ({
      name = "go";
      channel = "codedown";
      args = {
        packages = ["rand"];
      };
    })
  ];

  otherPackages = [];
}
