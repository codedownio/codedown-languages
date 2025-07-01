{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "clojure";

  kernels.clojure.enable = true;
}
