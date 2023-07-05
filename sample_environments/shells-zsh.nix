{ codedown
, channels ? {}
, overlays ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "shells-zsh";
  inherit channels overlays;
  # metaOnly = true;

  kernels = [];

  otherPackages = [
    { channel = "codedown"; attr = "shells.zsh"; contents = codedown.shells.zsh; }
  ];
}
