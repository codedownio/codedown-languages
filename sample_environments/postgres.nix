{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
  kernels.postgres.enable = true;
}
