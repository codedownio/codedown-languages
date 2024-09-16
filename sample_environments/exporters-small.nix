{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
  exporters.nbconvert-exporters.enable = true;
}
