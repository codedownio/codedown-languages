{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "postgres";

  kernels.postgres.enable = true;
}
