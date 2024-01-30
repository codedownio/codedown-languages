{ codedown
, channels ? {}
, overlays ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "shells-zsh";
  inherit channels overlays;

  kernels = [];

  otherPackages = [
    { channel = "codedown"; attr = "shells.zsh"; contents = codedown.shells.zsh; }
  ];
}
