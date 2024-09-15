{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment channels {
  kernels.ruby.enable = true;
}
