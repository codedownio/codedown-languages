{ codedown
, channels ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  environmentName = "shells-zsh";
  inherit channels;

  kernels = [];

  otherPackages = [
    { channel = "codedown"; attr = "shells.zsh"; contents = codedown.shells.zsh; }
  ];
}
