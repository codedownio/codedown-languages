{ codedown
, channels ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "cpp17";
  inherit channels;

  kernels = [
    ({
      name = "cpp17";
      channel = "codedown";
      args = {
        packages = [];
        attrs = ["cpp17" "cpp"];
      };
    })
  ];

  otherPackages = [];
}
