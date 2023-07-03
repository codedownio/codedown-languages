{ codedown
, channels ? {}
, overlays ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "clojure";
  inherit channels overlays;
  # metaOnly = true;

  kernels = [
    ({
      name = "clojure";
      channel = "codedown";
      args = {
        packages = [];
      };
    })
  ];

  otherPackages = [];
}
