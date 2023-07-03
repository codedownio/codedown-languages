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
