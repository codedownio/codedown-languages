{ codedown
, channels ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "exporters-large";
  inherit channels;

  kernels = [];

  otherPackages = [
    { channel = "codedown"; attr = "exporters.nbconvert-large"; contents = codedown.exporters.nbconvert-large; }
  ];
}
