{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
  exporters.nbconvert-exporters.enable = true;
  exporters.nbconvert-exporters.texliveScheme = "scheme-full";
}
