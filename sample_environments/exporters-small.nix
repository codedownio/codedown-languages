{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
  exporters.nbconvert.enable = true;
}
