{ codedown
, channels ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "spellchecker";
  inherit channels;

  kernels = [];

  otherPackages = [
    { channel = "codedown"; attr = "spellchecker"; contents = codedown.spellchecker; }
  ];
}
