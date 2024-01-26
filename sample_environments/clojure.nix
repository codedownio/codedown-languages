{ codedown
, channels ? {}
, overlays ? {}
, metaOnly ? false
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "clojure";
  inherit channels overlays metaOnly;

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
