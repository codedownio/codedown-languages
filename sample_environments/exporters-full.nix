{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment channels {
  exporters.nbconvert-exporters.enable = true;
  exporters.nbconvert-exporters.texliveScheme = "scheme-full";
}
