{ codedown
, channels ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "cpp23";
  inherit channels;

  kernels = [
    ({
      name = "cpp23";
      channel = "codedown";
      args = {
        packages = [];
        attrs = ["cpp23" "cpp"];
      };
    })
  ];

  otherPackages = [];
}
