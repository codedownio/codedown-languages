{ codedown
, channels ? {}
, overlays ? {}
, metaOnly ? false
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "exporters-large";
  inherit channels overlays metaOnly;

  kernels = [];

  otherPackages = [
    { channel = "codedown"; attr = "exporters.nbconvert-large"; contents = codedown.exporters.nbconvert-large; }
  ];
}
