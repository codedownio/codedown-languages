{ codedown
, channels ? {}
, overlays ? {}
, metaOnly ? false
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "shells-fish";
  inherit channels overlays metaOnly;

  kernels = [];

  otherPackages = [
    { channel = "codedown"; attr = "shells.fish"; contents = codedown.shells.fish; }
  ];
}
