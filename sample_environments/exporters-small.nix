{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment channels {
  exporters.nbconvert-exporters.enable = true;
}
