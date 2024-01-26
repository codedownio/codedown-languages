{ codedown
, channels ? {}
, overlays ? {}
, metaOnly ? false
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "shells-bash";
  inherit channels overlays metaOnly;

  kernels = [];

  otherPackages = [
    { channel = "codedown"; attr = "shells.bash"; contents = codedown.shells.bash; }
  ];
}
