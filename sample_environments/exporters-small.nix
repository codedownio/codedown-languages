{ codedown
, channels ? {}
, overlays ? {}
, metaOnly ? false
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "exporters-small";
  inherit channels overlays metaOnly;

  kernels = [];

  otherPackages = [
    { channel = "codedown"; attr = "exporters.nbconvert-small"; contents = codedown.exporters.nbconvert-small; }
  ];
}
