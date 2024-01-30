{ codedown
, channels ? {}
, overlays ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "shells-fish";
  inherit channels overlays;

  kernels = [];

  otherPackages = [
    { channel = "codedown"; attr = "shells.fish"; contents = codedown.shells.fish; }
  ];
}
