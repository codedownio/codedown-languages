{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment channels {
  kernels.clojure.enable = true;
}
