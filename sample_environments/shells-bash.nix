{ codedown
, channels ? {}
, overlays ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "shells-bash";
  inherit channels overlays;

  kernels = [];

  otherPackages = [
    { channel = "codedown"; attr = "shells.bash"; contents = codedown.shells.bash; }
  ];
}
