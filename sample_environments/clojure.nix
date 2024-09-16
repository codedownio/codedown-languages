{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
  kernels.clojure.enable = true;
}
