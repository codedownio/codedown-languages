{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
  kernels.bash.enable = true;

  packages = [
    channels.nixpkgs.htop
  ];
}
