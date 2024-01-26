{ codedown
, channels ? {}
, overlays ? {}
, metaOnly ? false
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "spellchecker";
  inherit channels overlays metaOnly;

  kernels = [];

  otherPackages = [
    { channel = "codedown"; attr = "spellchecker"; contents = codedown.spellchecker; }
  ];
}
