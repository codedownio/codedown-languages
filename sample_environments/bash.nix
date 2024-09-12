{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment channels {
  kernels.bash.enable = true;
}
