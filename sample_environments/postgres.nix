{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment channels {
  kernels.postgres.enable = true;
}
