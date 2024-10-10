{ codedown
, ...
}:

codedown.makeEnvironment {
  kernels.postgres.enable = true;
}
