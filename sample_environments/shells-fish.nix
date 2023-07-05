{ codedown
, channels ? {}
, overlays ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "shells-fish";
  inherit channels overlays;
  # metaOnly = true;

  kernels = [];

  otherPackages = [
    { channel = "codedown"; attr = "shells.fish"; contents = codedown.shells.fish; }
  ];
}
