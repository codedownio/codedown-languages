{ codedown
, ...
}:

codedown.makeEnvironment {
  exporters.nbconvert.enable = true;
}
