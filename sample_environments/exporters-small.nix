{ codedown
, channels ? {}
, overlays ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "exporters-small";
  inherit channels overlays;
  # metaOnly = true;

  kernels = [];

  otherPackages = [
    { channel = "codedown"; attr = "exporters.nbconvert-small"; contents = codedown.exporters.nbconvert-small; }
  ];
}
