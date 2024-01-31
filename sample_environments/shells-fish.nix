{ codedown
, channels ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "shells-fish";
  inherit channels;

  kernels = [];

  otherPackages = [
    { channel = "codedown"; attr = "shells.fish"; contents = codedown.shells.fish; }
  ];
}
