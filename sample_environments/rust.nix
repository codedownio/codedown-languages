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
      name = "rust";
      channel = "codedown";
      args = {
        packages = ["rand"];
      };
    })
  ];

  otherPackages = [];
}
