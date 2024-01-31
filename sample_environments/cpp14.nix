{ codedown
, channels ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "cpp14";
  inherit channels;

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
